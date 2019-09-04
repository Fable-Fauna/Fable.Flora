#r "paket: groupref netcorebuild //"
#load ".fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "Facades/netstandard"
#r "netstandard"
#endif

#nowarn "52"

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Tools.Git

module Util =

    let visitFile (visitor: string->string) (fileName : string) =
        File.ReadAllLines(fileName)
        |> Array.map (visitor)
        |> fun lines -> File.WriteAllLines(fileName, lines)

    let replaceLines (replacer: string->Match->string option) (reg: Regex) (fileName: string) =
        fileName |> visitFile (fun line ->
            let m = reg.Match(line)
            if not m.Success
            then line
            else
                match replacer line m with
                | None -> line
                | Some newLine -> newLine)



Target.create "Clean" (fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Seq.iter Shell.cleanDir
)

Target.create "Install" (fun _ ->
    ["src/Flora.CssProvider/Flora.CssProvider.fsproj"; "src/Flora.CssParser/Flora.CssParser.fsproj";]
    |> Seq.map IO.Path.GetDirectoryName 
    |> Seq.iter (fun x -> DotNet.restore id x)
)


Target.create "Build" (fun _ ->
    ["src/Flora.CssProvider/Flora.CssProvider.fsproj"; "src/Flora.CssParser/Flora.CssParser.fsproj"]
    |> Seq.map IO.Path.GetDirectoryName 
    |> Seq.iter (fun x ->DotNet.build id x)
)


Target.create "QuickBuild" (fun _ ->
    ["src/Flora.CssProvider/Flora.CssProvider.fsproj"; "src/Flora.CssParser/Flora.CssParser.fsproj"]
    |> Seq.map IO.Path.GetDirectoryName 
    |> Seq.iter (fun x -> DotNet.build (fun p -> {p with Configuration = DotNet.BuildConfiguration.Debug}) x )
)


// --------------------------------------------------------------------------------------
// Build a NuGet package
let needsPublishing (versionRegex: Regex) (releaseNotes: ReleaseNotes.ReleaseNotes) projFile =
    printfn "Project: %s" projFile
    if releaseNotes.NugetVersion.ToUpper().EndsWith("NEXT")
    then
        printfn "Version in Release Notes ends with NEXT, don't publish yet."
        false
    else
        File.ReadLines(projFile)
        |> Seq.tryPick (fun line ->
            let m = versionRegex.Match(line)
            if m.Success then Some m else None)
        |> function
            | None -> failwith "Couldn't find version in project file"
            | Some m ->
                let sameVersion = m.Groups.[1].Value = releaseNotes.NugetVersion
                if sameVersion then
                    printfn "Already version %s, no need to publish." releaseNotes.NugetVersion
                not sameVersion

//build assembly info
let toPackageReleaseNotes (notes: string list) =
    String.Join("\n * ", notes)
    |> (fun txt -> txt.Replace("\"", "\\\""))

//build nuspec
// let createNugetV2 (releaseNotes: ReleaseNotes.ReleaseNotes) (projFile: string) =
//     let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)
//     let nuspec = projFile.Remove(projFile.Length-6,6) + "nuspec"
//     let projDir = Path.GetDirectoryName(projFile)
    
//     (versionRegex, nuspec)
//     ||> Util.replaceLines (fun line _ ->
//                                 versionRegex.Replace(line, "<version>"+releaseNotes.NugetVersion+"</version>") |> Some)

//     (Regex("<releaseNotes>(.*?)</releaseNotes>", RegexOptions.IgnoreCase), nuspec)
//     ||> Util.replaceLines (fun line _ ->
//                                 versionRegex.Replace(line, "<releaseNotes>"+(toPackageReleaseNotes releaseNotes.Notes)+"</releaseNotes>") |> Some)                                                               

//     File.Copy(@"C:\Users\Orlando\Desktop\Projects2019\Fable-Fauna\Fable.Flora\src\Flora.CssProvider\Flora.CssProvider.nuspec",@"C:\Users\Orlando\Desktop\Projects2019\Fable-Fauna\Fable.Flora\src\Flora.CssProvider\obj\Release\Flora.CssProvider.nuspec")
//     let result =
//         DotNet.exec
//             (DotNet.Options.withWorkingDirectory projDir)
//             "pack"
//             @" Flora.CssProvider.fsproj -c Release --no-build -p:NuspecFile=Flora.CssProvider.nuspec"

//     if not result.OK then failwithf "dotnet pack failed with code %i" result.ExitCode




let createNuget (releaseNotes: ReleaseNotes.ReleaseNotes) (projFile: string) =
    let projDir = Path.GetDirectoryName(projFile)
    let result =
        DotNet.exec
            (DotNet.Options.withWorkingDirectory projDir)
            "pack"
            (sprintf "-c Release /p:PackageReleaseNotes=\"%s\"" (toPackageReleaseNotes releaseNotes.Notes))

    if not result.OK then failwithf "dotnet pack failed with code %i" result.ExitCode

let pushNuget (releaseNotes: ReleaseNotes.ReleaseNotes) (projFile: string) =
    let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)
    let projDir = Path.GetDirectoryName(projFile)

    // if needsPublishing versionRegex releaseNotes projFile then
    //     (versionRegex, projFile)
    //     ||> Util.replaceLines (fun line _ ->
    //                                 versionRegex.Replace(line, "<Version>"+releaseNotes.NugetVersion+"</Version>") |> Some)

    let nugetKey =
        match  Environment.environVarOrNone "NUGET_KEY" with
        | Some nugetKey -> nugetKey
        | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"
    Directory.GetFiles(projDir </> "bin" </> "Release", "*.nupkg")
    |> Array.find (fun nupkg -> nupkg.Contains(releaseNotes.NugetVersion))
    |> (fun nupkg ->
        Paket.push (fun p -> { p with ApiKey = nugetKey
                                      WorkingDir = Path.getDirectory nupkg }))


Target.create "ForcePush" (fun _ -> 
    let proj = "src/Flora.CssProvider/Flora.CssProvider.fsproj"
    let notes =  (IO.Path.GetDirectoryName proj) </> "RELEASE_NOTES.md" |> ReleaseNotes.load
    pushNuget notes proj
)

Target.create "CreateNugets" (fun _ ->
    let proj = "src/Flora.CssParser/Flora.CssParser.fsproj"
    let notes =  (IO.Path.GetDirectoryName proj) </> "RELEASE_NOTES.md" |> ReleaseNotes.load
    createNuget notes proj
)

Target.create "CreateNugets2" (fun _ ->
    let proj = "src/Flora.CssProvider/Flora.CssProvider.fsproj"
    let notes = (IO.Path.GetDirectoryName proj) </> "RELEASE_NOTES.md" |> ReleaseNotes.load
    createNuget notes proj
)

Target.create "PublishNugets" (fun _ ->
    ["src/Flora.CssProvider/Flora.CssProvider.fsproj"; "src/Flora.CssParser/Flora.CssParser.fsproj"]
    |> Seq.map (fun proj -> proj, (IO.Path.GetDirectoryName proj) </> "RELEASE_NOTES.md" |> ReleaseNotes.load)
    |> Seq.iter (fun (proj,notes) -> pushNuget notes proj)
)

// Where to push generated documentation


// Build order
"Clean"
    ==> "Install"
    ==> "Build"
    ==> "CreateNugets"
    ==> "CreateNugets2"
    //==> "PublishNugets"

// start build
Target.runOrDefault "Build"