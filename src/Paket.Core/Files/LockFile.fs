namespace Paket

open System
open System.Collections.Generic
open System.IO
open Paket.Domain
open Paket.Git.Handling
open Paket.Mercurial.Handling
open Paket.Logging
open Paket.PackageResolver
open Paket.ModuleResolver
open Paket.PackageSources
open Paket.Requirements
open Chessie.ErrorHandling

type LockFileGroup = {
    Name: GroupName
    Options:InstallOptions
    Resolution:PackageResolution
    RemoteFiles:ResolvedSourceFile list
}

module LockFileSerializer =
    /// [omit]
    let serializeOptionsAsLines options = [
        if options.Strict then yield "REFERENCES: STRICT"
        match options.Settings.GenerateLoadScripts with
        | Some true -> yield "GENERATE-LOAD-SCRIPTS: ON"
        | Some false -> yield "GENERATE-LOAD-SCRIPTS: OFF"
        | None -> ()
        match options.Redirects with
        | Some true -> yield "REDIRECTS: ON"
        | Some false -> yield "REDIRECTS: OFF"
        | None -> ()
        match options.ResolverStrategyForTransitives with
        | Some ResolverStrategy.Min -> yield "STRATEGY: MIN"
        | Some ResolverStrategy.Max -> yield "STRATEGY: MAX"
        | None -> ()
        match options.ResolverStrategyForDirectDependencies with
        | Some ResolverStrategy.Min -> yield "LOWEST_MATCHING: TRUE"
        | Some ResolverStrategy.Max -> yield "LOWEST_MATCHING: FALSE"
        | None -> () 
        match options.Settings.CopyLocal with
        | Some x -> yield "COPY-LOCAL: " + x.ToString().ToUpper()
        | None -> ()
        match options.Settings.CopyContentToOutputDirectory with
        | Some CopyToOutputDirectorySettings.Always -> yield "COPY-CONTENT-TO-OUTPUT-DIR: ALWAYS"
        | Some CopyToOutputDirectorySettings.Never -> yield "COPY-CONTENT-TO-OUTPUT-DIR: NEVER"
        | Some CopyToOutputDirectorySettings.PreserveNewest -> yield "COPY-CONTENT-TO-OUTPUT-DIR: PRESERVE-NEWEST"
        | None -> ()

        match options.Settings.ImportTargets with
        | Some x -> yield "IMPORT-TARGETS: " + x.ToString().ToUpper()
        | None -> ()

        match options.Settings.OmitContent with
        | Some ContentCopySettings.Omit -> yield "CONTENT: NONE"
        | Some ContentCopySettings.Overwrite -> yield "CONTENT: TRUE"
        | Some ContentCopySettings.OmitIfExisting -> yield "CONTENT: ONCE"
        | None -> ()

        match options.Settings.ReferenceCondition with
        | Some condition -> yield "CONDITION: " + condition.ToUpper()
        | None -> ()

        match options.Settings.FrameworkRestrictions |> getRestrictionList with
        | [] -> ()
        | list  -> yield "FRAMEWORK: " + (String.Join(", ",list)).ToUpper()]

    /// [omit]
    let serializePackages options (resolved : PackageResolution) = 
        let sources =
            resolved
            |> Seq.map (fun kv ->
                    let package = kv.Value
                    match package.Source with
                    | NuGetV2 source -> source.Url,source.Authentication,package
                    | NuGetV3 source -> source.Url,source.Authentication,package
                    | LocalNuGet(path,_) -> path,None,package
                )
            |> Seq.groupBy (fun (a,b,_) -> a,b)

        let all = 
            let hasReported = ref false
            [ yield! serializeOptionsAsLines options

              for (source, _), packages in sources do
                  if not !hasReported then
                    yield "NUGET"
                    hasReported := true

                  yield "  remote: " + String.quoted source

                  for _,_,package in packages |> Seq.sortBy (fun (_,_,p) -> p.Name) do
                      let versionStr = 
                          let s'' = package.Version.ToString()
                          let s' = 
                            if source.Contains "nuget.org" && options.Settings.IncludeVersionInPath <> Some true && package.Settings.IncludeVersionInPath <> Some true then 
                                package.Version.NormalizeToShorter() 
                            else 
                                s''

                          let s = if s''.Length > s'.Length then s' else s''
                          if s = "" then s else "(" + s + ")"

                      let settings =
                        if package.Settings.FrameworkRestrictions = options.Settings.FrameworkRestrictions then
                            { package.Settings with FrameworkRestrictions = FrameworkRestrictionList [] }
                        else
                            package.Settings
                      let s = settings.ToString().ToLower()

                      if s = "" then 
                        yield sprintf "    %O %s" package.Name versionStr 
                      else
                        yield sprintf "    %O %s - %s" package.Name versionStr s

                      for name,v,restrictions in package.Dependencies do
                          let versionStr = 
                              let s = v.ToString()
                              if s = "" then s else "(" + s + ")"

                          let restrictions = filterRestrictions options.Settings.FrameworkRestrictions restrictions |> getRestrictionList
                          if List.isEmpty restrictions || restrictions = getRestrictionList options.Settings.FrameworkRestrictions then
                            yield sprintf "      %O %s" name versionStr
                          else
                            yield sprintf "      %O %s - framework: %s" name versionStr (String.Join(", ",restrictions).ToLower())]
    
        String.Join(Environment.NewLine, all |> List.map (fun s -> s.TrimEnd()))

    let serializeSourceFiles (files:ResolvedSourceFile list) =
        let all =
            let updateHasReported = new List<Origin>()

            [ for (owner,project,origin), files in files |> List.groupBy (fun f -> f.Owner, f.Project, f.Origin) do
                match origin with
                | GitHubLink -> 
                    if not (updateHasReported.Contains(GitHubLink)) then
                        yield "GITHUB"
                        updateHasReported.Remove (HttpLink "") |> ignore
                        updateHasReported.Remove (GitLink (RemoteGitOrigin"")) |> ignore
                        updateHasReported.Remove (MercurialLink (RemoteMercurialOrigin "")) |> ignore
                        updateHasReported.Remove GistLink |> ignore
                        updateHasReported.Add GitHubLink
                    yield sprintf "  remote: %s/%s" owner project
                | GitLink (LocalGitOrigin  url)
                | GitLink (RemoteGitOrigin url) ->
                    if not (updateHasReported.Contains(GitLink(RemoteGitOrigin""))) then
                        yield "GIT"
                        updateHasReported.Remove GitHubLink |> ignore
                        updateHasReported.Remove GistLink |> ignore
                        updateHasReported.Remove (HttpLink "") |> ignore
                        updateHasReported.Remove (MercurialLink (RemoteMercurialOrigin "")) |> ignore
                        updateHasReported.Add (GitLink (RemoteGitOrigin""))
                    yield sprintf "  remote: " + url
                | MercurialLink (LocalMercurialOrigin  url)
                | MercurialLink (RemoteMercurialOrigin url) ->
                    if not (updateHasReported.Contains(MercurialLink(RemoteMercurialOrigin""))) then
                        yield "MERCURIAL"
                        updateHasReported.Remove (GitLink (RemoteGitOrigin"")) |> ignore
                        updateHasReported.Remove GitHubLink |> ignore
                        updateHasReported.Remove GistLink |> ignore
                        updateHasReported.Remove (HttpLink "") |> ignore
                        updateHasReported.Add (MercurialLink (RemoteMercurialOrigin ""))
                    yield sprintf "  remote: " + url
               
                | GistLink -> 
                    if not (updateHasReported.Contains(GistLink)) then
                        yield "GIST"
                        updateHasReported.Remove GitHubLink |> ignore
                        updateHasReported.Remove (HttpLink "") |> ignore
                        updateHasReported.Remove (GitLink (RemoteGitOrigin"")) |> ignore
                        updateHasReported.Remove (MercurialLink (RemoteMercurialOrigin "")) |> ignore
                        updateHasReported.Add GistLink
                    yield sprintf "  remote: %s/%s" owner project
                | HttpLink url ->
                    if not (updateHasReported.Contains(HttpLink(""))) then
                        yield "HTTP"
                        updateHasReported.Remove GitHubLink |> ignore
                        updateHasReported.Remove GistLink |> ignore
                        updateHasReported.Remove (GitLink (RemoteGitOrigin"")) |> ignore
                        updateHasReported.Remove (MercurialLink (RemoteMercurialOrigin "")) |> ignore
                        updateHasReported.Add (HttpLink "")
                    yield sprintf "  remote: " + url

                for file in files |> Seq.sortBy (fun f -> f.Owner.ToLower(),f.Project.ToLower(),f.Name.ToLower())  do

                    let path =
                        file.Name.TrimStart '/'
                        |> fun s ->
                            if System.Text.RegularExpressions.Regex.IsMatch (s, "\s") then
                                String.Concat ("\"", s, "\"")
                            else
                                s
                    match String.IsNullOrEmpty(file.Commit) with
                    | false ->
                        match file.AuthKey with
                        | Some authKey -> 
                            yield sprintf "    %s (%s) %s" path file.Commit authKey
                        | None -> 
                            yield sprintf "    %s (%s)" path file.Commit
                    | true -> 
                        match file.AuthKey with
                        | Some authKey -> yield sprintf "    %s %s" path authKey
                        | None -> yield sprintf "    %s" path

                    match file.Command with
                    | None -> ()
                    | Some command -> yield "      build: " + command

                    match file.PackagePath with
                    | None -> ()
                    | Some path -> yield "      path: " + path

                    match file.OperatingSystemRestriction with
                    | None -> ()
                    | Some filter -> yield "      os: " + filter

                    for (name,v) in file.Dependencies do
                        let versionStr = 
                            let s = v.ToString()
                            if s = "" then s else "(" + s + ")"
                        yield sprintf "      %O %s" name versionStr]

        String.Join(Environment.NewLine, all |> List.map (fun s -> s.TrimEnd()))

module LockFileParser =

    type ParseState =
        { GroupName : GroupName
          RepositoryType : string option
          RemoteUrl :string option
          Packages : ResolvedPackage list
          SourceFiles : ResolvedSourceFile list
          LastWasPackage : bool
          Options: InstallOptions }

    type private ParserOption =
    | ReferencesMode of bool
    | OmitContent of ContentCopySettings
    | ImportTargets of bool
    | GenerateLoadScripts of bool option
    | FrameworkRestrictions of FrameworkRestrictions
    | CopyLocal of bool
    | CopyContentToOutputDir of CopyToOutputDirectorySettings
    | Redirects of bool option
    | ReferenceCondition of string
    | DirectDependenciesResolverStrategy of ResolverStrategy option
    | TransitiveDependenciesResolverStrategy of ResolverStrategy option
    | Command of string
    | PackagePath of string
    | OperatingSystemRestriction of string

    let private (|Remote|NugetPackage|NugetDependency|SourceFile|RepositoryType|Group|InstallOption|) (state, line:string) =
        match (state.RepositoryType, line.Trim()) with
        | _, "HTTP" -> RepositoryType "HTTP"
        | _, "GIST" -> RepositoryType "GIST"
        | _, "GIT" -> RepositoryType "GIT"
        | _, "MERCURIAL" -> RepositoryType "MERCURIAL"
        | _, "NUGET" -> RepositoryType "NUGET"
        | _, "GITHUB" -> RepositoryType "GITHUB"
        | Some "NUGET", String.StartsWith "remote:" trimmed -> Remote(PackageSource.Parse("source " + trimmed.Trim()).ToString())
        | _, String.StartsWith "remote:" trimmed -> Remote(trimmed.Trim())
        | _, String.StartsWith "GROUP" trimmed -> Group(trimmed.Replace("GROUP","").Trim())
        | _, String.StartsWith "REFERENCES:" trimmed -> InstallOption(ReferencesMode(trimmed.Trim() = "STRICT"))
        | _, String.StartsWith "REDIRECTS:" trimmed -> 
            let setting =
                match trimmed.Trim().ToLowerInvariant() with
                | "on" -> Some true
                | "off" -> Some false
                | _ -> None

            InstallOption(Redirects(setting))
        | _, String.StartsWith "IMPORT-TARGETS:" trimmed -> InstallOption(ImportTargets(trimmed.Trim() = "TRUE"))
        | _, String.StartsWith "COPY-LOCAL:" trimmed -> InstallOption(CopyLocal(trimmed.Trim() = "TRUE"))
        | _, String.StartsWith "GENERATE-LOAD-SCRIPTS:" trimmed -> 
            let setting =
                match trimmed.Trim().ToLowerInvariant() with
                | "on" -> Some true
                | "off" -> Some false
                | _ -> None
                                            
            InstallOption(GenerateLoadScripts(setting))
        | _, String.StartsWith "COPY-CONTENT-TO-OUTPUT-DIR:" trimmed -> 
            let setting =
                match trimmed.Replace(":","").Trim().ToLowerInvariant() with
                | "always" -> CopyToOutputDirectorySettings.Always
                | "never" -> CopyToOutputDirectorySettings.Never
                | "preserve_newest" -> CopyToOutputDirectorySettings.PreserveNewest
                | x -> failwithf "Unknown copy_content_to_output_dir settings: %A" x
                                            
            InstallOption(CopyContentToOutputDir(setting))
        | _, String.StartsWith "FRAMEWORK:" trimmed -> InstallOption(FrameworkRestrictions(FrameworkRestrictionList (trimmed.Trim() |> Requirements.parseRestrictions true)))
        | _, String.StartsWith "CONDITION:" trimmed -> InstallOption(ReferenceCondition(trimmed.Trim().ToUpper()))
        | _, String.StartsWith "CONTENT:" trimmed -> 
            let setting =
                match trimmed.Trim().ToLowerInvariant() with
                | "none" -> ContentCopySettings.Omit
                | "once" -> ContentCopySettings.OmitIfExisting
                | _ -> ContentCopySettings.Overwrite

            InstallOption(OmitContent(setting))
        | _, String.StartsWith "STRATEGY:" trimmed -> 
            let setting =
                match trimmed.Trim().ToLowerInvariant() with
                | "min" -> Some ResolverStrategy.Min
                | "max" -> Some ResolverStrategy.Max
                | _ -> None

            InstallOption(TransitiveDependenciesResolverStrategy(setting))
        | _, String.StartsWith "LOWEST_MATCHING:" trimmed -> 
            let setting =
                match trimmed.Trim().ToLowerInvariant() with
                | "true" -> Some ResolverStrategy.Min
                | "false" -> Some ResolverStrategy.Max
                | _ -> None

            InstallOption(DirectDependenciesResolverStrategy(setting))
        | _, String.StartsWith "build: " trimmed ->
            InstallOption(Command trimmed)
        | _, String.StartsWith "path: " trimmed ->
            InstallOption(PackagePath trimmed)
        | _, String.StartsWith "os: " trimmed ->
            InstallOption(OperatingSystemRestriction trimmed)
        | _, trimmed when line.StartsWith "      " ->
            let frameworkSettings =
                if trimmed.Contains(" - ") then
                    let pos = trimmed.LastIndexOf(" - ")
                    try
                        InstallSettings.Parse(trimmed.Substring(pos + 3))
                    with
                    | _ -> InstallSettings.Parse("framework: " + trimmed.Substring(pos + 3)) // backwards compatible
                else
                    InstallSettings.Default
            if trimmed.Contains("(") then
                let parts = trimmed.Split '(' 
                NugetDependency (parts.[0].Trim(),parts.[1].Replace("(", "").Replace(")", "").Trim(),frameworkSettings)
            else
                if trimmed.Contains("  -") then
                    let pos = trimmed.IndexOf("  -")
                    NugetDependency (trimmed.Substring(0,pos),">= 0",frameworkSettings)
                else
                    NugetDependency (trimmed,">= 0",frameworkSettings)
        | Some "NUGET", trimmed -> NugetPackage trimmed
        | Some "GITHUB", trimmed -> SourceFile(GitHubLink, trimmed)
        | Some "GIST", trimmed -> SourceFile(GistLink, trimmed)
        | Some "GIT", trimmed -> SourceFile(GitLink(RemoteGitOrigin""), trimmed)
        | Some "MERCURIAL", trimmed -> SourceFile(MercurialLink(RemoteMercurialOrigin ""), trimmed)
        | Some "HTTP", trimmed  -> SourceFile(HttpLink(String.Empty), trimmed)
        | Some _, _ -> failwithf "unknown repository type %s." line
        | _ -> failwithf "unknown lock file format %s" line

    let private extractOption currentGroup option =
        match option with
        | ReferencesMode mode -> { currentGroup.Options with Strict = mode }
        | Redirects mode -> { currentGroup.Options with Redirects = mode }
        | ImportTargets mode -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with ImportTargets = Some mode } } 
        | CopyLocal mode -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with CopyLocal = Some mode }}
        | CopyContentToOutputDir mode -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with CopyContentToOutputDirectory = Some mode }}
        | FrameworkRestrictions r -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with FrameworkRestrictions = r }}
        | OmitContent omit -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with OmitContent = Some omit }}
        | GenerateLoadScripts mode -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with GenerateLoadScripts = mode }}
        | ReferenceCondition condition -> { currentGroup.Options with Settings = { currentGroup.Options.Settings with ReferenceCondition = Some condition }}
        | DirectDependenciesResolverStrategy strategy -> { currentGroup.Options with ResolverStrategyForDirectDependencies = strategy }
        | TransitiveDependenciesResolverStrategy strategy -> { currentGroup.Options with ResolverStrategyForTransitives = strategy }
        | _ -> failwithf "Unknown option %A" option

    let Parse(lockFileLines) =
        let remove textToRemove (source:string) = source.Replace(textToRemove, "")
        let removeBrackets = remove "(" >> remove ")"
        let parsePackage (s : string) =
            let parts = s.Split([|" - "|],StringSplitOptions.None)
            let optionsString = 
                if parts.Length < 2 then "" else 
                if parts.[1] <> "" && parts.[1].Contains(":") |> not then
                    ("framework: " + parts.[1])
                else
                    parts.[1]
            parts.[0],InstallSettings.Parse(optionsString)

        ([{ GroupName = Constants.MainDependencyGroup; RepositoryType = None; RemoteUrl = None; Packages = []; SourceFiles = []; Options = InstallOptions.Default; LastWasPackage = false }], lockFileLines)
        ||> Seq.fold(fun state line ->
            match state with
            | [] -> failwithf "error"
            | currentGroup::otherGroups ->
                if String.IsNullOrWhiteSpace line || line.Trim().StartsWith("specs:") then currentGroup::otherGroups else
                match (currentGroup, line) with
                | Remote(url) -> { currentGroup with RemoteUrl = Some url }::otherGroups
                | Group(groupName) -> { GroupName = GroupName groupName; RepositoryType = None; RemoteUrl = None; Packages = []; SourceFiles = []; Options = InstallOptions.Default; LastWasPackage = false } :: currentGroup :: otherGroups
                | InstallOption(Command(command)) -> 
                    let sourceFiles = 
                        match currentGroup.SourceFiles with
                        | sourceFile::rest ->{ sourceFile with Command = Some command } :: rest
                        |  _ -> failwith "missing source file"
                    { currentGroup with SourceFiles = sourceFiles }::otherGroups
                | InstallOption(PackagePath(path)) -> 
                    let sourceFiles = 
                        match currentGroup.SourceFiles with
                        | sourceFile::rest ->{ sourceFile with PackagePath = Some path } :: rest
                        |  _ -> failwith "missing source file"
                    { currentGroup with SourceFiles = sourceFiles }::otherGroups
                | InstallOption(OperatingSystemRestriction(filter)) -> 
                    let sourceFiles = 
                        match currentGroup.SourceFiles with
                        | sourceFile::rest ->{ sourceFile with OperatingSystemRestriction = Some filter } :: rest
                        |  _ -> failwith "missing source file"
                    { currentGroup with SourceFiles = sourceFiles }::otherGroups
                | InstallOption option -> 
                    { currentGroup with Options = extractOption currentGroup option }::otherGroups
                | RepositoryType repoType -> { currentGroup with RepositoryType = Some repoType }::otherGroups
                | NugetPackage details ->
                    match currentGroup.RemoteUrl with
                    | Some remote -> 
                        let package,settings = parsePackage details
                        let parts' = package.Split ' '
                        let version = 
                            if parts'.Length < 2 then
                                failwithf "No version specified for package %O in group %O." package currentGroup.GroupName
                            parts'.[1] |> removeBrackets

                        { currentGroup with 
                            LastWasPackage = true
                            Packages = 
                                    { Source = PackageSource.Parse(remote, None)
                                      Name = PackageName parts'.[0]
                                      Dependencies = Set.empty
                                      Unlisted = false
                                      Settings = settings
                                      Version = SemVer.Parse version } :: currentGroup.Packages }::otherGroups
                    | None -> failwith "no source has been specified."
                | NugetDependency (name, v, frameworkSettings) ->
                    let version,settings = parsePackage v
                    if currentGroup.LastWasPackage then
                        match currentGroup.Packages with
                        | currentPackage :: otherPackages -> 
                            { currentGroup with
                                    Packages = { currentPackage with
                                                    Dependencies = Set.add (PackageName name, DependenciesFileParser.parseVersionRequirement version, frameworkSettings.FrameworkRestrictions) currentPackage.Dependencies
                                                } :: otherPackages } ::otherGroups
                        | [] -> failwithf "cannot set a dependency to %s %s - no package has been specified." name v
                    else
                        match currentGroup.SourceFiles with
                        | currentFile :: rest -> 
                            { currentGroup with
                                    SourceFiles = 
                                        { currentFile with
                                                    Dependencies = Set.add (PackageName name, DependenciesFileParser.parseVersionRequirement version) currentFile.Dependencies
                                                } :: rest }  ::otherGroups
                        | [] -> failwithf "cannot set a dependency to %s %s- no remote file has been specified." name v
                | SourceFile(origin, details) ->
                    match origin with
                    | GitHubLink | GistLink ->
                        match currentGroup.RemoteUrl |> Option.map(fun s -> s.Split '/') with
                        | Some [| owner; project |] ->
                            let pieces =
                                if details.Contains "\"" then
                                    let pathInfo =
                                        match details.IndexOf ('"', 1) with
                                        | idx when idx >= 0 -> Some (details.Substring (1, idx - 1), idx)
                                        | _ -> None
                                    match pathInfo with
                                    | Some (path, pathEndIdx) ->
                                        let commitAndAuthKey = details.Substring(pathEndIdx + 2).Split(' ')
                                        Array.append [| path |] commitAndAuthKey
                                    | None -> Array.empty
                                else
                                    details.Split ' '

                            let path, commit, authKey =
                                match pieces with
                                | [| filePath; commit; authKey |] -> filePath, commit |> removeBrackets, (Some authKey)
                                | [| filePath; commit |] -> filePath, commit |> removeBrackets, None
                                | _ -> failwith "invalid file source details."
                            { currentGroup with
                                LastWasPackage = false
                                SourceFiles = { Commit = commit
                                                Owner = owner
                                                Origin = origin
                                                Project = project
                                                Dependencies = Set.empty
                                                Name = path
                                                Command = None
                                                OperatingSystemRestriction = None
                                                PackagePath = None
                                                AuthKey = authKey } :: currentGroup.SourceFiles }::otherGroups
                        | _ -> failwith "invalid remote details."
                    | HttpLink _ ->
                        match currentGroup.RemoteUrl |> Option.map(fun s -> s.Split '/' |> Array.toList) with
                        | Some [ protocol; _; domain; ] ->
                            let project, name, path, authKey = 
                                 match details.Split ' ' with
                                 | [| filePath; path |] -> "", filePath, path |> removeBrackets, None
                                 | [| filePath; path; authKey |] -> "", filePath, path |> removeBrackets, (Some authKey)
                                 | _ -> failwith "invalid file source details."
                        
                            let removeInvalidChars (str:string) = 
                                System.Text.RegularExpressions.Regex.Replace(str, "[:@\,]", "_")

                            let sourceFile =
                                { Commit = path
                                  Owner = domain |> removeInvalidChars
                                  Origin = HttpLink(currentGroup.RemoteUrl.Value)
                                  Project = project
                                  Dependencies = Set.empty
                                  Name = name
                                  Command = None
                                  OperatingSystemRestriction = None
                                  PackagePath = None
                                  AuthKey = authKey } 

                            { currentGroup with
                                LastWasPackage = false
                                SourceFiles = sourceFile :: currentGroup.SourceFiles }::otherGroups
                        | Some [ protocol; _; domain; project ] ->
                            { currentGroup with
                                LastWasPackage = false
                                SourceFiles = { Commit = String.Empty
                                                Owner = domain
                                                Origin = HttpLink(currentGroup.RemoteUrl.Value)
                                                Project = project
                                                Command = None
                                                OperatingSystemRestriction = None
                                                PackagePath = None
                                                Dependencies = Set.empty
                                                Name = details
                                                AuthKey = None } :: currentGroup.SourceFiles }::otherGroups
                        | Some (protocol :: _ :: domain :: project :: moredetails) ->
                            { currentGroup with
                                LastWasPackage = false
                                SourceFiles = { Commit = String.Empty
                                                Owner = domain
                                                Origin = HttpLink(currentGroup.RemoteUrl.Value)
                                                Project = project + "/" + String.Join("/",moredetails)
                                                Dependencies = Set.empty
                                                Command = None
                                                OperatingSystemRestriction = None
                                                PackagePath = None
                                                Name = details
                                                AuthKey = None } :: currentGroup.SourceFiles }::otherGroups
                        | _ ->  failwithf "invalid remote details %A" currentGroup.RemoteUrl
                    | GitLink _ ->
                        match currentGroup.RemoteUrl with
                        | Some cloneUrl ->
                            let owner,commit,project,origin,buildCommand,operatingSystemRestriction,packagePath = Git.Handling.extractUrlParts cloneUrl
                            { currentGroup with
                                LastWasPackage = false
                                SourceFiles = { Commit = details.Replace("(","").Replace(")","")
                                                Owner = owner
                                                Origin = GitLink origin
                                                Project = project
                                                Dependencies = Set.empty
                                                Command = buildCommand
                                                OperatingSystemRestriction = operatingSystemRestriction
                                                PackagePath = packagePath
                                                Name = "" 
                                                AuthKey = None } :: currentGroup.SourceFiles }::otherGroups
                        | _ ->  failwithf "invalid remote details %A" currentGroup.RemoteUrl
                    | MercurialLink _ ->
                        match currentGroup.RemoteUrl with
                        | Some cloneUrl ->
                            let owner,commit,project,origin,buildCommand,operatingSystemRestriction,packagePath = Mercurial.Handling.extractUrlParts cloneUrl
                            { currentGroup with
                                LastWasPackage = false
                                SourceFiles = { Commit = details.Replace("(","").Replace(")","")
                                                Owner = owner
                                                Origin = MercurialLink origin
                                                Project = project
                                                Dependencies = Set.empty
                                                Command = buildCommand
                                                OperatingSystemRestriction = operatingSystemRestriction
                                                PackagePath = packagePath
                                                Name = "" 
                                                AuthKey = None } :: currentGroup.SourceFiles }::otherGroups
                        | _ ->  failwithf "invalid remote details %A" currentGroup.RemoteUrl)


/// Allows to parse and analyze paket.lock files.
type LockFile(fileName:string,groups: Map<GroupName,LockFileGroup>) =
    member __.Groups = groups
    member __.FileName = fileName
    member __.RootPath = FileInfo(fileName).Directory.FullName
    member this.GetGroup groupName =
        match this.Groups |> Map.tryFind groupName with
        | Some g -> g
        | None -> failwithf "Group %O was not found in %s." groupName fileName

    member this.CheckIfPackageExistsInAnyGroup (packageName:PackageName) =
        match groups |> Seq.tryFind (fun g -> g.Value.Resolution.ContainsKey packageName) with
        | Some group -> sprintf "%sHowever, %O was found in group %O." Environment.NewLine packageName group.Value.Name
        | None -> ""
        

    /// Gets all dependencies of the given package
    member this.GetAllNormalizedDependenciesOf(groupName,package:PackageName,context) = 
        let group = groups.[groupName]
        let usedPackages = HashSet<_>()

        let rec addPackage (identity:PackageName) =
            match group.Resolution.TryFind identity with
            | Some package ->
                if usedPackages.Add(groupName,identity) then
                    if not group.Options.Strict then
                        for d,_,_ in package.Dependencies do
                            addPackage d
            | None ->
                failwithf "Package %O was referenced in %s, but it was not found in the paket.lock file in group %O.%s" identity context groupName (this.CheckIfPackageExistsInAnyGroup package)

        addPackage package

        usedPackages

    /// Gets all dependencies of the given package
    member this.GetAllDependenciesOf(groupName,package,context) =
        match this.GetAllDependenciesOfSafe(groupName,package) with
        | Some packages -> packages
        | None ->
            failwithf "Package %O was referenced in %s, but it was not found in the paket.lock file in group %O.%s" package context groupName (this.CheckIfPackageExistsInAnyGroup package)

    /// Gets all dependencies of the given package in the given group.
    member this.GetAllDependenciesOfSafe(groupName:GroupName,package) =
        let group = groups.[groupName]
        let allDependenciesOf package =
            let usedPackages = HashSet<_>()

            let rec addPackage packageName =
                let identity = packageName
                match group.Resolution.TryFind identity with
                | Some package ->
                    if usedPackages.Add packageName then
                        if not group.Options.Strict then
                            for d,_,_ in package.Dependencies do
                                addPackage d
                | None -> ()

            addPackage package

            usedPackages

        match group.Resolution |> Map.tryFind package with
        | Some v -> Some(allDependenciesOf v.Name)
        | None -> None
        
    member this.GetTransitiveDependencies(groupName) =
        let collectDependenciesForGroup group = 
            let fromNuGets =
                group.Resolution 
                |> Seq.map (fun d -> d.Value.Dependencies |> Seq.map (fun (n,_,_) -> n))
                |> Seq.concat
                |> Set.ofSeq

            let fromSourceFiles =
                group.RemoteFiles
                |> Seq.map (fun d -> d.Dependencies |> Seq.map fst)
                |> Seq.concat
                |> Set.ofSeq

            Set.union fromNuGets fromSourceFiles

        let group = groups.TryFind groupName
        match group with
        | None -> Set.empty
        | Some group -> collectDependenciesForGroup group
            

    member this.GetTopLevelDependencies(groupName) = 
        match groups |> Map.tryFind groupName with
        | None -> Map.empty
        | Some group ->
            let transitive = this.GetTransitiveDependencies groupName

            group.Resolution
            |> Map.filter (fun name _ -> transitive.Contains name |> not)

    member this.GetGroupedResolution() =
        this.Groups
        |> Seq.map (fun kv -> kv.Value.Resolution |> Seq.map (fun kv' -> (kv.Key,kv'.Key),kv'.Value))
        |> Seq.concat
        |> Map.ofSeq

    override __.ToString() =
        String.Join
            (Environment.NewLine,
             [|let mainGroup = groups.[Constants.MainDependencyGroup]
               yield LockFileSerializer.serializePackages mainGroup.Options mainGroup.Resolution
               yield LockFileSerializer.serializeSourceFiles mainGroup.RemoteFiles
               for g in groups do 
                if g.Key <> Constants.MainDependencyGroup then
                    yield "GROUP " + g.Value.Name.ToString()
                    yield LockFileSerializer.serializePackages g.Value.Options g.Value.Resolution
                    yield LockFileSerializer.serializeSourceFiles g.Value.RemoteFiles|])


    /// Updates the paket.lock file with the analyzed dependencies from the paket.dependencies file.
    member this.Save() =
        let output = this.ToString()

        let hasChanged =
            if File.Exists fileName then
                let text = File.ReadAllText(fileName)
                normalizeLineEndings output <> normalizeLineEndings text
            else true

        if hasChanged then
            File.WriteAllText(fileName, output)
            tracefn "Locked version resolution written to %s" fileName
        else
            tracefn "%s is already up-to-date" fileName
        hasChanged

    /// Parses a paket.lock file from file
    static member LoadFrom(lockFileName) : LockFile =
        LockFile.Parse(lockFileName, File.ReadAllLines lockFileName)

    /// Parses a paket.lock file from lines
    static member Parse(lockFileName,lines) : LockFile =
        try
            let groups =
                LockFileParser.Parse lines
                |> List.map (fun state ->
                    state.GroupName,
                    { Name = state.GroupName
                      Options = state.Options
                      Resolution = state.Packages |> Seq.fold (fun map p -> Map.add p.Name p map) Map.empty
                      RemoteFiles = List.rev state.SourceFiles })
                |> Map.ofList

            LockFile(lockFileName, groups)
        with
        | exn ->
            failwithf "Error during parsing of %s.%sMessage: %s" lockFileName Environment.NewLine exn.Message

    member this.GetPackageHull(referencesFile:ReferencesFile) =
        let usedPackages = Dictionary<_,_>()

        for g in referencesFile.Groups do
            match this.Groups |> Map.tryFind g.Key with
            | None -> failwithf "Group %O was referenced in %s, but not found in paket.lock."  g.Key referencesFile.FileName
            | Some lockGroup ->
                for p in g.Value.NugetPackages do
                    let k = g.Key,p.Name
                    if usedPackages.ContainsKey k |> not then
                        usedPackages.Add(k,p)

                for r in g.Value.RemoteFiles do
                    match lockGroup.RemoteFiles |> List.tryFind (fun x -> x.Name = r.Name) with
                    | None -> failwithf "Remote file %O was referenced in %s, but not found in paket.lock."  r.Name referencesFile.FileName
                    | Some lockRemote ->
                        for p,_ in lockRemote.Dependencies do
                            let k = g.Key,p
                            if usedPackages.ContainsKey k |> not then
                                usedPackages.Add(k,PackageInstallSettings.Default(p.ToString()))

        for g in referencesFile.Groups do
            g.Value.NugetPackages
            |> List.iter (fun package -> 
                try
                    for d in this.GetAllDependenciesOf(g.Key,package.Name,referencesFile.FileName) do
                        let k = g.Key,d
                        if usedPackages.ContainsKey k |> not then
                            usedPackages.Add(k,package)
                with exn -> failwithf "%s - in %s" exn.Message referencesFile.FileName)

        usedPackages

    member this.GetRemoteReferencedPackages(referencesFile:ReferencesFile,installGroup:InstallGroup) =
        [for r in installGroup.RemoteFiles do
            match this.Groups |> Map.tryFind installGroup.Name with
            | None -> failwithf "Group %O was referenced in %s, but not found in paket.lock."  installGroup.Name referencesFile.FileName
            | Some lockGroup ->
                match lockGroup.RemoteFiles |> List.tryFind (fun x -> x.Name.EndsWith(r.Name)) with
                | None -> failwithf "Remote file %O was referenced in %s, but not found in paket.lock."  r.Name referencesFile.FileName
                | Some lockRemote ->
                    for p,_ in lockRemote.Dependencies do
                        yield PackageInstallSettings.Default(p.ToString())]

    member this.GetPackageHull(groupName,referencesFile:ReferencesFile) =
        let usedPackages = Dictionary<_,_>()
        match referencesFile.Groups |> Map.tryFind groupName with
        | Some g ->
            for p in g.NugetPackages do
                let k = groupName,p.Name
                if usedPackages.ContainsKey k then
                    failwithf "Package %O is referenced more than once in %s within group %O." p.Name referencesFile.FileName groupName
                usedPackages.Add(k,p)

            g.NugetPackages
            |> List.iter (fun package ->
                try
                    for d in this.GetAllDependenciesOf(groupName,package.Name,referencesFile.FileName) do
                        let k = groupName,d
                        if usedPackages.ContainsKey k |> not then
                            usedPackages.Add(k,package)
                with exn -> failwithf "%s - in %s" exn.Message referencesFile.FileName)
        | None -> ()

        usedPackages

    member this.GetDependencyLookupTable() = 
        groups
        |> Seq.map (fun kv ->
                kv.Value.Resolution
                |> Seq.map (fun kv' -> 
                                (kv.Key,kv'.Key),
                                this.GetAllDependenciesOf(kv.Key,kv'.Value.Name,this.FileName)
                                |> Set.ofSeq
                                |> Set.remove kv'.Value.Name))
        |> Seq.concat
        |> Map.ofSeq

    member this.GetPackageHullSafe(referencesFile,groupName) =
        match referencesFile.Groups |> Map.tryFind groupName with
        | None -> Result.Succeed(Set.empty)
        | Some group ->
            group.NugetPackages
            |> Seq.map (fun package ->
                this.GetAllDependenciesOfSafe(groupName,package.Name)
                |> failIfNone (ReferenceNotFoundInLockFile(referencesFile.FileName, groupName.ToString(), package.Name)))
            |> collect
            |> lift (Seq.concat >> Set.ofSeq)
    
    