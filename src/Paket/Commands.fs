module Paket.Commands

open System

open Argu

type AddArgs =
    | [<CustomCommandLine("nuget")>][<Mandatory>] Nuget of package_id:string
    | [<CustomCommandLine("version")>] Version of version:string
    | [<CustomCommandLine("project")>] Project of name:string
    | [<CustomCommandLine("group")>] Group of name:string
    | [<AltCommandLine("-f")>] Force
    | [<AltCommandLine("-i")>] Interactive
    | Redirects
    | CreateNewBindingFiles
    | Clean_Redirects
    | No_Install
    | Keep_Major
    | Keep_Minor
    | Keep_Patch
    | Touch_Affected_Refs
    | Keep_Unknown_Packages
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Nuget(_) -> "NuGet package id."
            | Group(_) -> "Add the package to the given group. If omitted the Main group is used."
            | Version(_) -> "Allows to specify version of the package."
            | Project(_) -> "Allows to add the package to a single project only."
            | Force -> "Forces the download and reinstallation of all packages."
            | Interactive -> "Asks the user for every project if he or she wants to add the package to the projects's paket.references file."
            | Redirects -> "Creates binding redirects for the NuGet packages."
            | CreateNewBindingFiles -> "Creates binding redirect files if needed."
            | Clean_Redirects -> "Removes all binding redirects that are not specified by Paket."
            | No_Install -> "Skips paket install process (patching of csproj, fsproj, ... files) after the generation of paket.lock file."
            | Keep_Major -> "Allows only updates that are not changing the major version of the NuGet packages."
            | Keep_Minor -> "Allows only updates that are not changing the minor version of the NuGet packages."
            | Keep_Patch -> "Allows only updates that are not changing the patch version of the NuGet packages."
            | Touch_Affected_Refs -> "Touches project files referencing packages which are affected, to help incremental build tools detecting the change."
            | Keep_Unknown_Packages -> "Ignores unknown packages during garbage collection. Useful when managing NuGet packages with nuget.exe."

type ConfigArgs =
    | [<CustomCommandLine("add-credentials")>] AddCredentials of string
    | [<CustomCommandLine("add-token")>] AddToken of string * string
    | Username of string
    | Password of string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | AddCredentials(_) -> "Add credentials for the specified NuGet feed."
            | AddToken(_) -> "Add token for the specified source."
            | Username(_) -> "Provide a username (for scripting)"
            | Password(_) -> "provide a password on the commandline (for scripting)"

type ConvertFromNugetArgs =
    | [<AltCommandLine("-f")>] Force
    | No_Install
    | No_Auto_Restore
    | Creds_Migration of mode:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Force -> "Forces the conversion, even if paket.dependencies or paket.references files are present."
            | No_Install -> "Skips paket install process (patching of csproj, fsproj, ... files) after the generation of paket.lock file."
            | No_Auto_Restore -> "Skips paket auto-restore process afterward generation of dependencies / references files."
            | Creds_Migration(_) -> "Specify a mode for migrating NuGet source credentials. Possible values are [`encrypt`|`plaintext`|`selective`]. The default mode is `encrypt`."

type FindRefsArgs =
    | [<CustomCommandLine("group")>] Group of name:string
    | [<CustomCommandLine("nuget")>][<ExactlyOnce>] Packages of package_name:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Group(_) -> "Allows to specify a group. If omitted the Main group is used."
            | Packages(_) -> "List of packages."

type InitArgs =
    | [<Hidden>] NoArg
with
    interface IArgParserTemplate with
        member __.Usage = ""

type AutoRestoreFlags = On | Off

type AutoRestoreArgs =
    | [<MainCommand; Mandatory>] Flags of AutoRestoreFlags
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Flags _ -> "enables or disables auto restore for the repo."

type InstallArgs =
    | [<AltCommandLine("-f")>] Force
    | Redirects
    | CreateNewBindingFiles
    | Clean_Redirects
    | Keep_Major
    | Keep_Minor
    | Keep_Patch
    | [<CustomCommandLine("--generate-load-scripts")>] Generate_Load_Scripts
    | [<CustomCommandLine("--only-referenced")>] Install_Only_Referenced
    | [<CustomCommandLine("project-root")>] Project_Root of target:string
    | [<CustomCommandLine("load-script-framework")>] Load_Script_Framework of target:string
    | [<CustomCommandLine("load-script-type")>] Load_Script_Type of id:string
    | Touch_Affected_Refs
    | Keep_Unknown_Packages
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Force -> "Forces the download and reinstallation of all packages."
            | Redirects -> "Creates binding redirects for the NuGet packages."
            | CreateNewBindingFiles -> "Creates binding redirect files if needed."
            | Clean_Redirects -> "Removes all binding redirects that are not specified by Paket."
            | Install_Only_Referenced -> "Only install packages that are referenced in paket.references files, instead of all packages in paket.dependencies."
            | Generate_Load_Scripts -> "Allows to generate C# and F# include scripts which references installed packages in a interactive environment like F# Interactive or ScriptCS."
            | Keep_Major -> "Allows only updates that are not changing the major version of the NuGet packages."
            | Keep_Minor -> "Allows only updates that are not changing the minor version of the NuGet packages."
            | Keep_Patch -> "Allows only updates that are not changing the patch version of the NuGet packages."
            | Touch_Affected_Refs -> "Touches project files referencing packages which are affected, to help incremental build tools detecting the change."
            | Project_Root _ -> "Alternative project root [only used for tooling]."
            | Load_Script_Framework _ -> "Framework identifier to generate scripts for, such as net45 or net4."
            | Load_Script_Type _ -> "Language to generate scripts for, must be one of 'fsx' or 'csx'."
            | Keep_Unknown_Packages -> "Ignores unknown packages during garbage collection. Useful when managing NuGet packages with nuget.exe."

type OutdatedArgs =
    | Ignore_Constraints
    | [<CustomCommandLine("group")>] Group of name:string
    | [<AltCommandLine("--pre")>] Include_Prereleases
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Ignore_Constraints -> "Ignores the version requirement as in the paket.dependencies file."
            | Group(_) -> "Just check for one group."
            | Include_Prereleases -> "Includes prereleases."

type RemoveArgs =
    | [<CustomCommandLine("nuget")>][<Mandatory>] Nuget of package_id:string
    | [<CustomCommandLine("project")>] Project of name:string
    | [<CustomCommandLine("group")>] Group of name:string
    | [<AltCommandLine("-f")>] Force
    | [<AltCommandLine("-i")>] Interactive
    | No_Install
    | Keep_Unknown_Packages
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Nuget(_) -> "NuGet package id."
            | Group(_) -> "Removes the package from the given group. If omitted the Main group is used."
            | Project(_) -> "Allows to remove the package from a single project only."
            | Force -> "Forces the download and reinstallation of all packages."
            | Interactive -> "Asks the user for every project if he or she wants to remove the package from the projects's paket.references file. By default every installation of the package is removed."
            | No_Install -> "Skips paket install process (patching of csproj, fsproj, ... files) after the generation of paket.lock file."
            | Keep_Unknown_Packages -> "Ignores unknown packages during garbage collection. Useful when managing NuGet packages with nuget.exe."


type ClearCacheArgs =
    | [<Hidden>] NoArg
with
    interface IArgParserTemplate with
        member __.Usage = ""

type RestoreArgs =
    | [<AltCommandLine("-f")>] Force
    | [<CustomCommandLine("--only-referenced")>] Install_Only_Referenced
    | [<CustomCommandLine("--touch-affected-refs")>] Touch_Affected_Refs
    | [<CustomCommandLine("--ignore-checks")>] Ignore_Checks
    | [<CustomCommandLine("--fail-on-checks")>] Fail_On_Checks
    | Keep_Unknown_Packages
    | [<CustomCommandLine("group")>] Group of name:string
    | [<Unique>] Project of file_name:string
    | [<Unique>] References_Files of file_name:string list
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Force -> "Forces the download of all packages."
            | Group(_) -> "Allows to restore a single group."
            | Install_Only_Referenced -> "Allows to restore packages that are referenced in paket.references files, instead of all packages in paket.dependencies."
            | Touch_Affected_Refs -> "Touches project files referencing packages which are being restored, to help incremental build tools detecting the change."
            | Ignore_Checks -> "Skips the test if paket.dependencies and paket.lock are in sync."
            | Fail_On_Checks -> "Causes the restore to fail if any of the checks fail."
            | Project(_) -> "Allows to restore dependencies for a project."
            | Keep_Unknown_Packages -> "Ignores unknown packages during garbage collection. Useful when managing NuGet packages with nuget.exe."
            | References_Files(_) -> "Allows to restore all packages from the given paket.references files."

type SimplifyArgs =
    | [<AltCommandLine("-i")>] Interactive
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Interactive -> "Asks to confirm to delete every transitive dependency from each of the files."

type UpdateArgs =
    | [<CustomCommandLine("nuget")>] Nuget of package_id:string
    | [<CustomCommandLine("version")>] Version of version:string
    | [<CustomCommandLine("group")>] Group of name:string
    | [<AltCommandLine("-f")>] Force
    | Redirects
    | CreateNewBindingFiles
    | Clean_Redirects
    | No_Install
    | Keep_Major
    | Keep_Minor
    | Keep_Patch
    | Filter
    | Touch_Affected_Refs
    | Keep_Unknown_Packages
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Nuget(_) -> "NuGet package id."
            | Group(_) -> "Allows to specify the dependency group."
            | Version(_) -> "Allows to specify version of the package."
            | Force -> "Forces the download and reinstallation of all packages."
            | Redirects -> "Creates binding redirects for the NuGet packages."
            | CreateNewBindingFiles -> "Creates binding redirect files if needed."
            | Clean_Redirects -> "Removes all binding redirects that are not specified by Paket."
            | No_Install -> "Skips paket install process (patching of csproj, fsproj, ... files) after the generation of paket.lock file."
            | Keep_Major -> "Allows only updates that are not changing the major version of the NuGet packages."
            | Keep_Minor -> "Allows only updates that are not changing the minor version of the NuGet packages."
            | Keep_Patch -> "Allows only updates that are not changing the patch version of the NuGet packages."
            | Filter -> "Treat the nuget parameter as a regex to filter packages rather than an exact match."
            | Touch_Affected_Refs -> "Touches project files referencing packages which are affected, to help incremental build tools detecting the change."
            | Keep_Unknown_Packages -> "Ignores unknown packages during garbage collection. Useful when managing NuGet packages with nuget.exe."

type FindPackagesArgs =
    | [<CustomCommandLine("searchtext")>] SearchText of text:string
    | [<CustomCommandLine("source")>] Source of source_feed:string
    | [<CustomCommandLine("max")>] MaxResults of int
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | SearchText(_) -> "Search text of a Package."
            | Source(_) -> "Allows to specify the package source feed."
            | MaxResults(_) -> "Maximum number of results."

            
type FixNuspecArgs =
    | [<CustomCommandLine("file")>] File of text:string
    | [<CustomCommandLine("references-file")>] ReferencesFile of text:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File(_) -> "FileName of the nuspec file."
            | ReferencesFile(_) -> "FileName of the nuspec file."

type ShowInstalledPackagesArgs =
    | All
    | [<CustomCommandLine("project")>] Project of string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "Shows all installed packages (incl. transitive dependencies)."
            | Project(_) -> "Show only packages that are installed in the given project."

type ShowGroupsArgs =
    | [<Hidden; NoCommandLine>] PlaceHolder
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | PlaceHolder -> "Doesn't trace other output than installed packages."

type FindPackageVersionsArgs =
    | [<CustomCommandLine("name"); Hidden>] Name of package_id:string
    | [<CustomCommandLine("nuget")>] NuGet of package_id:string
    | [<CustomCommandLine("source")>] Source of source_feed:string
    | [<CustomCommandLine("max")>] MaxResults of int
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Name(_) -> "Name of the package. [DEPRECATED]"
            | NuGet(_) -> "Name of the NuGet package."
            | Source(_) -> "Allows to specify the package source feed."
            | MaxResults(_) -> "Maximum number of results."

type PackArgs =
    | [<CustomCommandLine("output")>][<Mandatory>] Output of path:string
    | [<CustomCommandLine("buildconfig")>] BuildConfig of config_name:string
    | [<CustomCommandLine("buildplatform")>] BuildPlatform of target:string
    | [<CustomCommandLine("version")>] Version of version:string
    | [<CustomCommandLine("templatefile")>] TemplateFile of path:string
    | [<CustomCommandLine("exclude")>] ExcludedTemplate of templateId:string
    | [<CustomCommandLine("specific-version")>] SpecificVersion of templateId:string * version:string
    | [<CustomCommandLine("releaseNotes")>] ReleaseNotes of text:string
    | [<CustomCommandLine("lock-dependencies")>] LockDependencies
    | [<CustomCommandLine("minimum-from-lock-file")>] LockDependenciesToMinimum
    | [<CustomCommandLine("pin-project-references")>] PinProjectReferences
    | [<CustomCommandLine("symbols")>] Symbols
    | [<CustomCommandLine("include-referenced-projects")>] IncludeReferencedProjects
    | [<CustomCommandLine("project-url")>] ProjectUrl of url:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Output(_) -> "Output directory to put .nupkg files."
            | BuildConfig(_) -> "Optionally specify build configuration that should be packaged (defaults to Release)."
            | BuildPlatform(_) -> "Optionally specify build platform that should be packaged (if not provided or empty, checks all known platform targets)."
            | Version(_) -> "Specify version of the package."
            | TemplateFile(_) -> "Allows to specify a single template file."
            | ExcludedTemplate(_) -> "Exclude template file by id."
            | SpecificVersion(_) -> "Specifies a version number for template with given id."
            | ReleaseNotes(_) -> "Specify relase notes for the package."
            | LockDependencies -> "Get the version requirements from paket.lock instead of paket.dependencies."
            | LockDependenciesToMinimum -> "Get the version requirements from paket.lock instead of paket.dependencies, and add them as a minimum version.  `lock-dependencies` will over-ride this option."
            | PinProjectReferences -> "Pin dependencies generated from project references (=) instead of using minimum (>=) for version specification.  If `lock-dependencies` is specified, project references will be pinned even if this option is not specified."
            | Symbols -> "Build symbol/source packages in addition to library/content packages."
            | IncludeReferencedProjects -> "Include symbol/source from referenced projects."
            | ProjectUrl(_) -> "Url to the projects home page."

type PushArgs =
    | [<CustomCommandLine("url")>][<Mandatory>] Url of url:string
    | [<CustomCommandLine("file")>][<Mandatory>] FileName of path:string
    | [<CustomCommandLine("apikey")>] ApiKey of key:string
    | [<CustomCommandLine("endpoint")>] EndPoint of path:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Url(_) -> "Url of the NuGet feed."
            | FileName(_) -> "Path to the package."
            | ApiKey(_) -> "Optionally specify your API key on the command line. Otherwise uses the value of the `nugetkey` environment variable."
            | EndPoint(_) -> "Optionally specify a custom api endpoint to push to. Defaults to `/api/v2/package`."

type GenerateLoadScriptsArgs = 
    | [<CustomCommandLine("framework")>] Framework of target:string
    | [<CustomCommandLine("type")>] ScriptType of id:string
with
  interface IArgParserTemplate with
      member this.Usage = 
        match this with
        | Framework _ -> "Framework identifier to generate scripts for, such as net4 or netcore."
        | ScriptType _ -> "Language to generate scripts for, must be one of 'fsx' or 'csx'."
  
type WhyArgs =
    | [<CustomCommandLine("nuget")>][<Mandatory>] NuGet of package_id:string
    | [<CustomCommandLine("group")>] Group of name:string
    | Details
with
  interface IArgParserTemplate with
      member this.Usage = 
        match this with
        | NuGet _ -> "Name of the NuGet package."
        | Group _ -> "Allows to specify the dependency group."
        | Details -> "Display detailed info with all possible paths, versions and framework constraints."

type Command =
    // global options
    | [<AltCommandLine("-v"); Inherit>]                 Verbose
    | [<Inherit>]                                       Log_File of path:string
    | [<AltCommandLine("-s"); Inherit>]                 Silent
    | [<Inherit>]                                       Version
    | [<Inherit;Hidden>]                                From_Bootstrapper
    // subcommands
    | [<CustomCommandLine("add")>]                      Add of ParseResults<AddArgs>
    | [<CustomCommandLine("clear-cache")>]              ClearCache of ParseResults<ClearCacheArgs>
    | [<CustomCommandLine("config")>]                   Config of ParseResults<ConfigArgs>
    | [<CustomCommandLine("convert-from-nuget")>]       ConvertFromNuget of ParseResults<ConvertFromNugetArgs>
    | [<CustomCommandLine("find-refs")>]                FindRefs of ParseResults<FindRefsArgs>
    | [<CustomCommandLine("init")>]                     Init of ParseResults<InitArgs>
    | [<CustomCommandLine("auto-restore")>]             AutoRestore of ParseResults<AutoRestoreArgs>
    | [<CustomCommandLine("install")>]                  Install of ParseResults<InstallArgs>
    | [<CustomCommandLine("outdated")>]                 Outdated of ParseResults<OutdatedArgs>
    | [<CustomCommandLine("remove")>]                   Remove of ParseResults<RemoveArgs>
    | [<CustomCommandLine("restore")>]                  Restore of ParseResults<RestoreArgs>
    | [<CustomCommandLine("simplify")>]                 Simplify of ParseResults<SimplifyArgs>
    | [<CustomCommandLine("update")>]                   Update of ParseResults<UpdateArgs>
    | [<CustomCommandLine("find-packages")>]            FindPackages of ParseResults<FindPackagesArgs>
    | [<CustomCommandLine("find-package-versions")>]    FindPackageVersions of ParseResults<FindPackageVersionsArgs>
    | [<CustomCommandLine("fix-nuspec")>]               FixNuspec of ParseResults<FixNuspecArgs>
    | [<CustomCommandLine("show-installed-packages")>]  ShowInstalledPackages of ParseResults<ShowInstalledPackagesArgs>
    | [<CustomCommandLine("show-groups")>]              ShowGroups of ParseResults<ShowGroupsArgs>
    | [<CustomCommandLine("pack")>]                     Pack of ParseResults<PackArgs>
    | [<CustomCommandLine("push")>]                     Push of ParseResults<PushArgs>
    | [<CustomCommandLine("generate-include-scripts")>] GenerateIncludeScripts of ParseResults<GenerateLoadScriptsArgs> // backward compatibility
    | [<CustomCommandLine("generate-load-scripts")>]    GenerateLoadScripts of ParseResults<GenerateLoadScriptsArgs>
    | [<CustomCommandLine("why")>]                      Why of ParseResults<WhyArgs>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Add _ -> "Adds a new package to your paket.dependencies file."
            | ClearCache _ -> "Clears the NuGet and git cache folders."
            | Config _ -> "Allows to store global configuration values like NuGet credentials."
            | ConvertFromNuget _ -> "Converts from using NuGet to Paket."
            | FindRefs _ -> "Finds all project files that have the given NuGet packages installed."
            | Init _ -> "Creates an empty paket.dependencies file in the working directory."
            | AutoRestore _ -> "Enables or disables automatic Package Restore in Visual Studio during the build process."
            | Install _ -> "Download the dependencies specified by the paket.dependencies or paket.lock file into the `packages/` directory and update projects."
            | Outdated _ -> "Lists all dependencies that have newer versions available."
            | Remove _ -> "Removes a package from your paket.dependencies file and all paket.references files."
            | Restore _ -> "Download the dependencies specified by the paket.lock file into the `packages/` directory."
            | Simplify _ -> "Simplifies your paket.dependencies file by removing transitive dependencies."
            | Update _ -> "Update one or all dependencies to their latest version and update projects."
            | FindPackages _ -> "Allows to search for packages."
            | FindPackageVersions _ -> "Allows to search for package versions."
            | FixNuspec _ -> "Allows to patch a nuspec with the correct dependencies."
            | ShowInstalledPackages _ -> "Shows all installed top-level packages."
            | ShowGroups _ -> "Shows all groups."
            | Pack _ -> "Packs all paket.template files within this repository."
            | Push _ -> "Pushes the given `.nupkg` file."
            | GenerateIncludeScripts _ -> "Obsolete, see generate-load-scripts."
            | GenerateLoadScripts _ -> "Allows to generate C# and F# include scripts which references installed packages in a interactive environment like F# Interactive or ScriptCS."
            | Why _ -> "Prints user-friendly reason for referencing a specified package"
            | Log_File _ -> "Specify a log file for the paket process."
            | Silent -> "Suppress console output for the paket process."
            | Verbose -> "Enable verbose console output for the paket process." 
            | Version -> "Display the version." 
            | From_Bootstrapper -> "Call coming from the '--run' feature of the bootstrapper." 

let commandParser = ArgumentParser.Create<Command>(programName = "paket", errorHandler = new ProcessExiter())

let markdown (subParser : ArgumentParser) (width : int) (additionalText : string) =
    let (afterCommandText, afterOptionsText) =
        let ensureLineBreak (text : string) = if String.IsNullOrEmpty(text) then text else text + Environment.NewLine + Environment.NewLine
        let cleanUp (text : string) = text.Replace("# [after-command]", "")
                                          .Replace("# [after-options]", "")
                                          .Trim('\r', '\n') |> ensureLineBreak
        let afterCommandIndex = additionalText.IndexOf("# [after-command]")
        let afterOptionsIndex = additionalText.IndexOf("# [after-options]")
        
        if afterCommandIndex = -1 then "", additionalText |> cleanUp
        else if afterOptionsIndex = -1 then additionalText |> cleanUp, ""
        else (additionalText.Substring(0, afterCommandIndex) |> cleanUp, additionalText.Substring(afterOptionsIndex) |> cleanUp)

    let parentMetadata = subParser.ParentInfo |> Option.get

    let indentBy spaces (text:string) =
        let whitespace = String(' ', spaces)
        text.Split([|Environment.NewLine|], StringSplitOptions.None)
        |> Seq.map (fun line -> whitespace + line)
        |> String.concat Environment.NewLine

    let replace (pattern : string) (replacement : string) input =
        System.Text.RegularExpressions.Regex.Replace(input, pattern, replacement)

    let syntax = 
        subParser.PrintCommandLineSyntax(usageStringCharacterWidth = width)
        |> indentBy 4

    let options = subParser.PrintUsage(hideSyntax=true, usageStringCharacterWidth = width)

    System.Text.StringBuilder()
        .Append("# paket ")
        .AppendLine(parentMetadata.Name)
        .AppendLine()
        .AppendLine(parentMetadata.Description)
        .AppendLine()
        .AppendLine("    [lang=console]")
        .AppendLine(syntax)
        .AppendLine()
        .Append(afterCommandText)
        .Append(options)
        .Append(afterOptionsText)
        .ToString()

let getAllCommands () = commandParser.GetSubCommandParsers()
