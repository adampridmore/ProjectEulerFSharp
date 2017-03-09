msbuild ..\MyFSharpHelpers.fsproj /P:Config=Release
nuget pack ..\MyFSharpHelpers.fsproj

# This doesn't seem to like relative paths.
nuget push *.nupkg -source "C:\Users\Adam\Dropbox\Work\Dev\NugetPackages\MyFSharpHelpers"

del *.nupkg