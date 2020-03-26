#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Testing.ReportGenerator //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.Testing

let isLocal = Environment.environVarAsBoolOrDefault "GITHUB_ACTIONS" true

let artifactDir = "./artifacts"
let binaryDir = artifactDir </> "Binaries"
let packageDir = artifactDir </> "Packages"
let testResultDir = artifactDir </> "TestResults"
let testCoverageReportDir = artifactDir </> "TestCoverageReport"

let launchDefaultProgram filePath =
  CreateProcess.fromRawCommand "cmd" ["/C"; "start"; "\"\""; Path.getFullName filePath]
  |> Proc.run
  |> ignore

Target.create "Clean" <| fun _ ->
  Shell.cleanDir artifactDir

let configureBuildOptions (o: DotNet.BuildOptions) =
  { o with
      Configuration = DotNet.Release;
      MSBuildParams = {
        o.MSBuildParams with
          Verbosity = Some Quiet;
          Properties = o.MSBuildParams.Properties @ [
            "PackageOutputPath", Path.getFullName packageDir
          ];
          Targets = "rebuild;pack"::o.MSBuildParams.Targets };
      OutputPath = Some binaryDir; }

Target.create "Build" <| fun _ ->
  DotNet.build configureBuildOptions "."

let configureTestOptions (o: DotNet.TestOptions) =
  { o with
      Collect = Some "XPlat Code Coverage";
      Configuration = DotNet.Release;
      MSBuildParams = { o.MSBuildParams with Verbosity = Some Quiet };
      NoBuild = true;
      Output = Some binaryDir;
      ResultsDirectory = Some testResultDir; }

Target.create "Test" <| fun _ ->
  DotNet.test configureTestOptions "."
  !! (sprintf "%s/**/coverage.cobertura.xml" testResultDir)
  |> Shell.copyFiles testResultDir
  System.IO.Directory.EnumerateDirectories testResultDir
  |> Shell.deleteDirs

let configureReportOptions (p: ReportGenerator.ReportGeneratorParams) =
  { p with
      TargetDir = testCoverageReportDir;
      ToolType = ToolType.CreateLocalTool() }

Target.create "GenerateCoverageReport" <| fun _ ->
  !! (sprintf "%s/*.xml" testResultDir)
  |> Seq.toList
  |> ReportGenerator.generateReports configureReportOptions

Target.create "LaunchCoverageReport" <| fun _ ->
  testCoverageReportDir </> "index.htm" |> launchDefaultProgram

Target.create "Default" ignore

open Fake.Core.TargetOperators

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "GenerateCoverageReport"
  =?> ("LaunchCoverageReport", isLocal && Environment.isWindows)
  ==> "Default"

Target.runOrDefault "Default"
