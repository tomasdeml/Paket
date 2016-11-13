/// Contains helpers which allow to interact with [mercurial](http://mercurial-scm.org/) via the command line.
module Paket.Mercurial.CommandHelper

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Text
open System.Collections.Generic
open Paket.Logging
open Paket

/// Arguments on the Mono executable
let mutable monoArguments = ""

/// Modifies the ProcessStartInfo according to the platform semantics
// TODO DEDUP
let platformInfoAction (psi : ProcessStartInfo) =
    if isMonoRuntime && psi.FileName.EndsWith ".exe" then
        psi.Arguments <- monoArguments + " " + psi.FileName + " " + psi.Arguments
        psi.FileName <- monoPath
/// Specifies a global timeout for hg.exe - default is *no timeout*
let mutable mercurialTimeOut = TimeSpan.MaxValue

// Prefer Hg.exe from TortoiseHG as it ships with more features (e.g. working mercurial_keyring extension)
let private knownMercurialPaths = @"[ProgramFiles]\TortoiseHg\;[ProgramFilesX86]\TortoiseHg\;[ProgramFiles]\Mercurial\;[ProgramFilesX86]\Mercurial\"


/// Searches the given directories for all occurrences of the given file name
/// [omit]
// TODO DEDUP
let tryFindFile dirs file =
    let files =
        dirs
        |> Seq.map (fun (path : string) ->
               let dir =
                 DirectoryInfo(
                   path
                    .Replace("[ProgramFiles]",ProgramFiles)
                    .Replace("[ProgramFilesX86]",ProgramFilesX86)
                    .Replace("[SystemRoot]",SystemRoot))

               if not dir.Exists then ""
               else
                   let fi = FileInfo(Path.Combine(dir.FullName,file))
                   if fi.Exists then fi.FullName
                   else "")
        |> Seq.filter ((<>) "")
        |> Seq.cache
    if not (Seq.isEmpty files) then Some(Seq.head files)
    else None

/// Searches the given directories for the given file, failing if not found.
/// [omit]
// TODO DEDUP
let findFile dirs file =
    match tryFindFile dirs file with
    | Some found -> found
    | None -> failwithf "%s not found in %A." file dirs

/// Retrieves the environment variable or None
// TODO DEDUP
let environVarOrNone name =
    let var = Environment.GetEnvironmentVariable name
    if String.IsNullOrEmpty var then None
    else Some var

/// Splits the entries of an environment variable and removes the empty ones.
// TODO DEDUP
let splitEnvironVar name =
    let var = environVarOrNone name
    if var = None then [ ]
    else var.Value.Split([| Path.PathSeparator |]) |> Array.toList


/// Detects whether the given path does not contains invalid characters.
// TODO DEDUP
let isValidPath (path:string) =
    let containsInvalidChar =
        Path.GetInvalidPathChars()
        |> Array.exists (fun char -> path.Contains(char.ToString()))
    (not containsInvalidChar)

/// Gets the list of valid directories included in the PATH environment variable.
// TODO DEDUP
let pathDirectories =
    splitEnvironVar "PATH"
    |> Seq.map (fun value -> value.Trim())
    |> Seq.filter (String.IsNullOrEmpty >> not)
    |> Seq.filter isValidPath

/// Searches the current directory and the directories within the PATH
/// environment variable for the given file. If successful returns the full
/// path to the file.
/// ## Parameters
///  - `file` - The file to locate
// TOD DEDUP
let tryFindFileOnPath (file : string) : string option =
    pathDirectories
    |> Seq.append [ "." ]
    |> fun path -> tryFindFile path file

/// Returns the AppSettings for the key - Splitted on ;
/// [omit]
// TODO DEDUP
let appSettings (key : string) (fallbackValue : string) =
    let value =
        let setting =
#if NETSTANDARD1_6
            null : string
#else
            try
                System.Configuration.ConfigurationManager.AppSettings.[key]
            with exn -> ""
#endif
        if not (String.IsNullOrWhiteSpace setting) then setting
        else fallbackValue
    value.Split([| ';' |], StringSplitOptions.RemoveEmptyEntries)

/// Tries to find the tool via AppSettings. If no path has the right tool we are trying the PATH system variable.
/// [omit]
// TODO DEDUP
let tryFindPath settingsName fallbackValue tool =
    let paths = appSettings settingsName fallbackValue
    match tryFindFile paths tool with
    | Some path -> Some path
    | None -> tryFindFileOnPath tool

/// Tries to find the tool via AppSettings. If no path has the right tool we are trying the PATH system variable.
/// [omit]
// TODO DEDUP
let findPath settingsName fallbackValue tool =
    match tryFindPath settingsName fallbackValue tool with
    | Some file -> file
    | None -> tool

/// Tries to locate the hg.exe via the eviroment variable "GIT".
let hgExePath =
    if Utils.isUnix then
        "hg"
    else
        let ev = Environment.GetEnvironmentVariable "HG"
        if not (String.IsNullOrWhiteSpace ev) then ev else findPath "MercurialPath" knownMercurialPaths "hg.exe"

/// [omit]
// TODO DEDUP
let startedProcesses = HashSet()

/// [omit]
// TODO DEDUP
let start (proc : Process) =
    if isMonoRuntime && proc.StartInfo.FileName.ToLowerInvariant().EndsWith(".exe") then
        proc.StartInfo.Arguments <- "--debug \"" + proc.StartInfo.FileName + "\" " + proc.StartInfo.Arguments
        proc.StartInfo.FileName <- monoPath

    proc.Start() |> ignore
    startedProcesses.Add(proc.Id, proc.StartTime) |> ignore

/// Runs the given process and returns the exit code.
/// ## Parameters
///
///  - `configProcessStartInfoF` - A function which overwrites the default ProcessStartInfo.
///  - `timeOut` - The timeout for the process.
///  - `silent` - If this flag is set then the process output is redirected to the given output functions `errorF` and `messageF`.
///  - `errorF` - A function which will be called with the error log.
///  - `messageF` - A function which will be called with the message log.
// TODO DEDUP
let ExecProcessWithLambdas configProcessStartInfoF (timeOut : TimeSpan) silent errorF messageF =
    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    configProcessStartInfoF proc.StartInfo
    platformInfoAction proc.StartInfo
    if String.IsNullOrEmpty proc.StartInfo.WorkingDirectory |> not then
        if Directory.Exists proc.StartInfo.WorkingDirectory |> not then
            failwithf "Start of process %s failed. WorkingDir %s does not exist." proc.StartInfo.FileName
                proc.StartInfo.WorkingDirectory
    if silent then
        proc.StartInfo.RedirectStandardOutput <- true
        proc.StartInfo.RedirectStandardError <- true
        proc.ErrorDataReceived.Add(fun d ->
            if not <| (isNull d.Data) then errorF d.Data)
        proc.OutputDataReceived.Add(fun d ->
            if not <| (isNull d.Data) then messageF d.Data)
    try
        start proc
    with exn -> failwithf "Start of process %s failed. %s" proc.StartInfo.FileName exn.Message
    if silent then
        proc.BeginErrorReadLine()
        proc.BeginOutputReadLine()
    if timeOut = TimeSpan.MaxValue then proc.WaitForExit()
    else
        if not <| proc.WaitForExit(int timeOut.TotalMilliseconds) then
            try
                proc.Kill()
            with exn ->  ()
            failwithf "Process %s %s timed out." proc.StartInfo.FileName proc.StartInfo.Arguments
    proc.ExitCode


/// A process result including error code, message log and errors.
// TODO DEDUP
type ProcessResult =
    { ExitCode : int
      Messages : List<string>
      Errors : List<string> }
    member x.OK = x.ExitCode = 0
    static member New exitCode messages errors =
        { ExitCode = exitCode
          Messages = messages
          Errors = errors }

/// Runs the given process and returns the process result.
/// ## Parameters
///
///  - `configProcessStartInfoF` - A function which overwrites the default ProcessStartInfo.
///  - `timeOut` - The timeout for the process.
// TODO DEDUP
let ExecProcessAndReturnMessages configProcessStartInfoF timeOut =
    let errors = new List<_>()
    let messages = new List<_>()
    let exitCode = ExecProcessWithLambdas configProcessStartInfoF timeOut true (errors.Add) (messages.Add)
    ProcessResult.New exitCode messages errors

/// Converts a sequence of strings to a string with delimiters
// TODO DEDUP
let inline separated delimiter (items : string seq) = String.Join(delimiter, Array.ofSeq items)

/// Converts a sequence of strings into a string separated with line ends
// TODO DEDUP
let inline toLines text = separated Environment.NewLine text

/// Runs hg.exe with the given command in the given repository directory.
let runMercurialCommand repositoryDir command =
    let processResult =
        ExecProcessAndReturnMessages (fun info ->
          info.FileName <- hgExePath
          info.WorkingDirectory <- repositoryDir
          info.Arguments <- command) mercurialTimeOut

    processResult.OK,processResult.Messages,toLines processResult.Errors

/// [omit]
let runMercurialCommandf fmt = Printf.ksprintf runMercurialCommand fmt

/// [omit]
let getMercurialResult repositoryDir command =
    let _,msg,_ = runMercurialCommand repositoryDir command
    msg

/// Starts the given process and returns immediatly.
// TODO DEDUP
let fireAndForget configProcessStartInfoF =
    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    configProcessStartInfoF proc.StartInfo
    try
        start proc
    with exn -> failwithf "Start of process %s failed. %s" proc.StartInfo.FileName exn.Message

/// Fires the given mercurial command ind the given repository directory and returns immediatly.
let fireAndForgetMercurialCommand repositoryDir command =
    fireAndForget (fun info ->
      info.FileName <- hgExePath
      info.WorkingDirectory <- repositoryDir
      info.Arguments <- command)

/// Runs the given process, waits for its completion and returns if it succeeded.
// TODO DEDUP
let directExec configProcessStartInfoF =
    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    configProcessStartInfoF proc.StartInfo
    try
        start proc
    with exn -> failwithf "Start of process %s failed. %s" proc.StartInfo.FileName exn.Message
    proc.WaitForExit()
    proc.ExitCode = 0

/// Runs the given mercurial command, waits for its completion and returns whether it succeeded.
let directRunMercurialCommand repositoryDir command =
    directExec (fun info ->
      info.FileName <- hgExePath
      info.WorkingDirectory <- repositoryDir
      info.Arguments <- command)

/// Runs the given mercurial command, waits for its completion.
let mercurialCommand repositoryDir command =
    let ok,msg,error = runMercurialCommand repositoryDir command

    if not ok then failwith error else
    if verbose then
        msg |> Seq.iter (tracefn "%s")

/// [omit]
let mercurialCommandf repositoryDir fmt = Printf.ksprintf (mercurialCommand repositoryDir) fmt

/// Runs the mercurial command and returns the results.
let runFullMercurialCommand repositoryDir command =
    try
        let ok,msg,errors = runMercurialCommand repositoryDir command

        let errorText = toLines msg + Environment.NewLine + errors
        if errorText.Contains "fatal: " || errorText.Contains "abort: " then
            failwith errorText

        if msg.Count = 0 then [||] else
        if verbose then
            msg |> Seq.iter (tracefn "%s")
        msg |> Seq.toArray
    with
    | exn -> failwithf "Could not run \"mercurial %s\".\r\nError: %s" command exn.Message

/// Runs the mercurial command and returns the first line of the result.
let runSimpleMercurialCommand repositoryDir command =
    try
        let ok,msg,errors = runMercurialCommand repositoryDir command

        let errorText = toLines msg + Environment.NewLine + errors
        if errorText.Contains "fatal: " || errorText.Contains "abort: " then
            failwith errorText

        if msg.Count = 0 then "" else
        if verbose then
            msg |> Seq.iter (tracefn "%s")
        msg.[0]
    with
    | exn -> failwithf "Could not run \"mercurial %s\".\r\nError: %s" command exn.Message