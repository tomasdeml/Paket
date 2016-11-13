module Paket.Mercurial.Handling
open Paket.Utils
open System
open System.IO
open System.Text.RegularExpressions
open Paket.Logging
open Paket

type MercurialLinkOrigin =
| RemoteMercurialOrigin of string
| LocalMercurialOrigin  of string

let extractUrlParts (mercurialConfig:string) =
    let isOperator operator = VersionRange.BasicOperators |> List.exists ((=) operator)
    let url,commit,options = 
        match mercurialConfig.Split([|' '|],StringSplitOptions.RemoveEmptyEntries) |> List.ofArray with
        | part::_ ->
            let rest = mercurialConfig.Substring(part.Length)
            let parts = 
                [ let current = Text.StringBuilder()
                  let quoted = ref false
                  for x in rest do
                    if x = '"' then
                        quoted := not !quoted

                    if (x = ':' || x = ' ') && not !quoted then
                        yield current.ToString()
                        if x = ':' then
                            yield ":"
                        current.Clear() |> ignore
                    else
                        current.Append x |> ignore
                
                  yield current.ToString()]
                |> List.filter (String.IsNullOrWhiteSpace >> not)

            match parts with
            | k::colon::_ when colon = ":" && (k.ToLower() = "build" || k.ToLower() = "os" || k.ToLower() = "packages")  -> 
                part,None,rest
            | operator::version::operator2::version2::prerelease::options when 
                    isOperator operator && isOperator operator2 && 
                      not (prerelease.ToLower() = "build" || prerelease.ToLower() = "os" || prerelease.ToLower() = "packages" || prerelease = ":") 
               -> 
                let startPos = mercurialConfig.Substring(part.Length).IndexOf(prerelease)
                let options = mercurialConfig.Substring(part.Length + startPos + prerelease.Length)
                part,Some(operator + " " + version + " " + operator2 + " " + version2 +  " " + prerelease),options
            | operator::version::prerelease::options when 
                    isOperator operator && not (isOperator prerelease) && 
                      not (prerelease.ToLower() = "build" || prerelease.ToLower() = "os" || prerelease.ToLower() = "packages" || prerelease = ":") 
               -> 
                let startPos = mercurialConfig.Substring(part.Length).IndexOf(prerelease)
                let options = mercurialConfig.Substring(part.Length + startPos + prerelease.Length)
                part,Some(operator + " " + version +  " " + prerelease),options
            | operator::version::operator2::version2::options when isOperator operator && isOperator operator2 -> 
                let startPos = mercurialConfig.Substring(part.Length).IndexOf(version2)
                let options = mercurialConfig.Substring(part.Length + startPos + version2.Length)
                part,Some(operator + " " + version + " " + operator2 + " " + version2),options
            | operator::version::options when isOperator operator -> 
                let startPos = mercurialConfig.Substring(part.Length).IndexOf(version)
                let options = mercurialConfig.Substring(part.Length + startPos + version.Length)
                part,Some(operator + " " + version),options
            | commit::options -> 
                let startPos = mercurialConfig.Substring(part.Length).IndexOf(commit)
                let options = mercurialConfig.Substring(part.Length + startPos + commit.Length)
                part,Some (commit.Trim('\"')),options
            | _ -> mercurialConfig,None,""
        | _ -> mercurialConfig,None,""
    
    let kvPairs = parseKeyValuePairs options

    let buildCommand = 
        match kvPairs.TryGetValue "build" with
        | true,x -> Some (x.Trim('\"'))
        | _ -> None

    let operatingSystemRestriction = 
        match kvPairs.TryGetValue "os" with
        | true,x -> Some (x.Trim('\"'))
        | _ -> None

    let packagePath = 
        match kvPairs.TryGetValue "packages" with
        | true,x -> Some (x.Trim('\"'))
        | _ -> None

    let url = url.TrimEnd '/'
    let url = 
        match url.Split ' ' |> Array.toList with 
        | [url; commit] -> url
        | _ -> url

    let origin =
        match url with
        | String.StartsWith @"file:///" _ ->
            LocalMercurialOrigin url
        | _ ->
            RemoteMercurialOrigin url

    let server =
        match origin with
        | LocalMercurialOrigin _ ->
            "localfilesystem"
        | _ ->
            match url.Replace(":","/").LastIndexOf('/') with 
            | -1 -> url
            | pos -> url.Substring(0, pos)
                        
    let server = 
        match server.IndexOf("://") with
        | -1 -> server
        | pos -> server.Substring(pos + 3).Replace(":","") |> removeInvalidChars
        |> fun s -> s.Replace(":","/").TrimStart('/')

    let project = url.Substring(url.LastIndexOf('/')+1)
    let project = if Directory.Exists project then Path.GetFileName project else project

    server,commit,project,origin,buildCommand,operatingSystemRestriction,packagePath

let stripUncommittedChangesFlag (hash: string) =
    hash.TrimEnd ('+')

let getHash url commit =
    try
        let revParam = if not <| (String.IsNullOrEmpty commit) then (sprintf "--rev %s" (quote commit)) else null
        // No other way to get full hash without --debug (as the 'log' command does not accept url), let's hope its output is stable across hg versions
        let outputLines = CommandHelper.runFullMercurialCommand "" (sprintf "id --id --debug %s %s" revParam (quote url))
        if Array.isEmpty outputLines then
            failwith "No hash returned"
        else
            let hash = outputLines |> Array.last 
            stripUncommittedChangesFlag hash
    with
    | exn -> failwithf "Could not find hash for %s in '%s'%sMessage: %s" commit url Environment.NewLine exn.Message

let getCurrentHash repoFolder = 
    getHash repoFolder "." 

let fetchCache repoCacheFolder cloneUrl =
    try
        if not <| Directory.Exists repoCacheFolder then
            if not <| Directory.Exists Constants.MercurialRepoCacheFolder then
                Directory.CreateDirectory Constants.MercurialRepoCacheFolder |> ignore
            tracefn "Cloning %s to %s" cloneUrl repoCacheFolder
            CommandHelper.runSimpleMercurialCommand Constants.MercurialRepoCacheFolder (sprintf "clone %s --pull --noupdate" (quote cloneUrl)) |> ignore
        else
            verbosefn "Fetching %s to %s" cloneUrl repoCacheFolder 
            CommandHelper.runSimpleMercurialCommand repoCacheFolder "pull --force" |> ignore
    with
    | exn -> failwithf "Fetching the mercurial cache at %s failed.%sMessage: %s" repoCacheFolder Environment.NewLine exn.Message

let setDefaultPath repoFolder url = 
    let utf8WithoutBom = new System.Text.UTF8Encoding(false)
    let hgrcPath = Path.Combine(repoFolder, ".hg", "hgrc")
    let hgrcContent = File.ReadAllText (hgrcPath, utf8WithoutBom)
    let hgrcContentWithUpstream = Regex.Replace (hgrcContent, @"^default\s*=\s*.+$", (sprintf "default = %s" url), RegexOptions.IgnoreCase ||| RegexOptions.Multiline)
    File.WriteAllText (hgrcPath, hgrcContentWithUpstream, utf8WithoutBom) 

let checkoutToPaketFolder repoFolder cloneUrl cacheCloneUrl commit =
    try
        if Directory.Exists repoFolder then
            verbosefn "Fetching %s to %s" cacheCloneUrl repoFolder 
            CommandHelper.runSimpleMercurialCommand repoFolder (sprintf "pull %s --force" (quote cacheCloneUrl)) |> ignore
        else
            let destination = DirectoryInfo(repoFolder).Parent.FullName
            if not <| Directory.Exists destination then
                Directory.CreateDirectory destination |> ignore
            verbosefn "Cloning %s to %s" cacheCloneUrl repoFolder
            CommandHelper.runSimpleMercurialCommand destination (sprintf "clone %s --pull" (quote cacheCloneUrl)) |> ignore
            setDefaultPath repoFolder cloneUrl

        tracefn "Setting %s to %s" repoFolder commit
        CommandHelper.runSimpleMercurialCommand repoFolder (sprintf "update --check %s" (quote commit)) |> ignore
    with
    | exn -> failwithf "Checkout to %s failed.%sMessage: %s" repoFolder Environment.NewLine exn.Message