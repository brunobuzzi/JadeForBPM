"Boot a DPRO development image"
| list |
SessionManager current splashShowAtStartup: false.
SmalltalkSystemShell allInstances first 
	position: 20@50;
	yourself.
ClassBrowserAbstract allSubclasses do: [:each | each plugins add: DebugInfoPlugin].
IdeaSpaceShell show
	absorbCard: SmalltalkWorkspaceDocument show;
	absorbCard: PackageBrowserShell show;
	absorbCard: SystemBrowserShell show;
	position: 50@80; extent: 1200@700;
	yourself.
TestRunner show
	position: 900@40;
	yourself.
TranscriptShell show.
TranscriptShell allInstances first view position: 20@600.
Debugger defaultExtent: 1000@600.
SourceManager default fileIn: 'RegressionTestsLoad.st'.
list := #(
	'Core\Contributions\JGFoster\StoreString for Views.pax'
	'Core\Contributions\JGFoster\StoreString for Views Test.pax'
).
ProgressDialog showModalWhile: [:progress |
	1 to: list size do: [:i |
		| name percent |
		name := list at: i.
		percent := (i * 100 / list size) asInteger.
		progress
			value: percent;
			text: 'Loading package: ' , name;
			yourself.
		(File exists: name) ifTrue: [PackageManager current install: name].
	].
].
Transcript clear.
Cursor wait showWhile: [
	SourceManager default compressSources.
	(Delay forMilliseconds: 200) wait.
	SessionManager current saveImage.
].
!
