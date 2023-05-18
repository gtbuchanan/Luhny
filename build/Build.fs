open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Testing

let isLocal = not <| Environment.environVarAsBool "GITHUB_ACTIONS"

let artifactDir = "./artifacts"
let binaryDir = artifactDir </> "Binaries"
let packageDir = artifactDir </> "Packages"
let testResultDir = artifactDir </> "TestResults"
let testCoverageReportDir = artifactDir </> "TestCoverageReport"

let launchDefaultProgram filePath =
  CreateProcess.fromRawCommand "cmd" ["/C"; "start"; "\"\""; Path.getFullName filePath]
  |> Proc.run
  |> ignore

let clean _ = Shell.cleanDir artifactDir

let configureBuildOptions (o: DotNet.BuildOptions) =
  { o with
      Configuration = DotNet.Release;
      MSBuildParams = {
        o.MSBuildParams with
          Verbosity = Some Quiet;
          Properties = o.MSBuildParams.Properties @ [
            "BaseOutputPath", Path.getFullName binaryDir + Path.directorySeparator
            "PackageOutputPath", Path.getFullName packageDir
          ];
          Targets = "rebuild;pack"::o.MSBuildParams.Targets } }

let build _ = DotNet.build configureBuildOptions "."

let configureTestOptions (o: DotNet.TestOptions) =
  { o with
      Collect = Some "XPlat Code Coverage";
      Configuration = DotNet.Release;
      MSBuildParams = {
        o.MSBuildParams with
          Verbosity = Some Quiet;
          Properties = o.MSBuildParams.Properties @ [
            "BaseOutputPath", Path.getFullName binaryDir + Path.directorySeparator
          ] };
      NoBuild = true;
      ResultsDirectory = Some testResultDir; }

let test _ =
  DotNet.test configureTestOptions "."
  !! (sprintf "%s/**/coverage.cobertura.xml" testResultDir)
  |> Shell.copyFiles testResultDir
  System.IO.Directory.EnumerateDirectories testResultDir
  |> Shell.deleteDirs

let configureReportOptions (p: ReportGenerator.ReportGeneratorParams) =
  { p with
      ReportTypes = [
        ReportGenerator.ReportType.Cobertura;
        ReportGenerator.ReportType.HtmlInline_AzurePipelines_Dark];
      TargetDir = testCoverageReportDir;
      ToolType = ToolType.CreateLocalTool() }

let generateCoverageReport _ =
  !! (sprintf "%s/*.xml" testResultDir)
  |> Seq.toList
  |> ReportGenerator.generateReports configureReportOptions

let launchCoverageReport _ = testCoverageReportDir </> "index.htm" |> launchDefaultProgram

let initTargets () =
  Target.create "Clean" clean
  Target.create "Build" build
  Target.create "Test" test
  Target.create "GenerateCoverageReport" generateCoverageReport
  Target.create "LaunchCoverageReport" launchCoverageReport
  Target.create "Default" ignore

  "Clean"
    ==> "Build"
    ==> "Test"
    ==> "GenerateCoverageReport"
    =?> ("LaunchCoverageReport", isLocal && Environment.isWindows)
    ==> "Default"
    |> ignore

let private setExecutionContext =
  Array.toList
  >> Context.FakeExecutionContext.Create false "build.fsx"
  >> Context.RuntimeContext.Fake
  >> Context.setExecutionContext

[<EntryPoint>]
let main argv =
  setExecutionContext argv
  initTargets ()
  Target.runOrDefault "Default"
  0
