"Boot a Jade development image"

SessionManager current splashShowAtStartup: false.
SmalltalkSystemShell allInstances first position: 20@50.
(TranscriptShell allInstances , #(nil)) first ifNotNil: [:shell | shell view position: 20@600; extent: 800@300; setFocus].
!
| list |
list := #(
	'..\Jade\sources\JGF Dolphin'
	'..\Jade\sources\JGF Fading Dialog'
	'..\Jade\sources\GemStone C Interface'
	'..\Jade\sources\GemStone Objects'
	'..\Jade\sources\GemStone Session'
	'..\Jade\sources\VisualWorks Component'
	'..\Jade\sources\Jade UI Base'
	'..\Jade\sources\Jade Class Browser'
	'..\Jade\sources\Jade Method Browser'
	'..\Jade\sources\Jade Inspector'
	'..\Jade\sources\Jade User Browser'
	'..\Jade\sources\Monticello'
	'..\Jade\sources\Jade UI'
	'..\Jade\sources\Jade Autocompletation'
	'..\Jade\sources\Jade Test Browser'
	'..\Jade\sources\Jade System Browser'
	'..\Jade\sources\Jade Transcript'
	'..\Jade\sources\Jade Process Browser'
	'..\Jade\sources\Object Log Browser'
	'..\Jade\sources\Jade Login'
	'..\Jade\sources\Jade Deployment'
	'..\Jade\sources\Contacts Demo'
	'..\Jade\sources\Jade in Dolphin'
	'..\Jade\sources\Jade Tests'
	'..\Jade\sources\Jade Tutorial'
	'sources\Jade Customization for BpmFlow'	
).
(Delay forMilliseconds: 200) wait.
ProgressDialog showModalWhile: [:progress |
	1 to: list size do: [:i |
		| name percent |
		name := list at: i.
		percent := (i * 100 / list size) asInteger.
		progress
			value: percent;
			text: 'Loading package: ' , name;
			yourself.
		PackageManager current install: name , '.pax'.
	].
].
!
IdeaSpaceShell allInstances isEmpty ifTrue: [
	IdeaSpaceShell show
		absorbCard: SmalltalkWorkspaceDocument show;
		absorbCard: PackageBrowserShell show;
		absorbCard: SystemBrowserShell show;
		position: 50@80; extent: 1200@700;
		yourself.
].
!
"Set window sizes and selection for Packages tab"
| container filterView layoutManager packagesView sources | 
container := (PackageBrowserShell allInstances first view viewNamed: 'packages') referee. 
filterView := container viewNamed: 'filter'. 
packagesView := container viewNamed: 'packages'. 
layoutManager := container layoutManager. 
layoutManager arrangementOf: filterView put: 1. 
layoutManager arrangementOf: packagesView put: 7. 
container layout. 
sources := filterView model asSet detect: [:each | each name = 'sources'].
filterView presenter selection: sources.
!
"Set window sizes and selection for System Browser tab"
| container filterView layoutManager packagesView sources | 
container := (SystemBrowserShell allInstances first view viewNamed: 'packages') referee. 
filterView := container viewNamed: 'filter'. 
packagesView := container viewNamed: 'packages'. 
layoutManager := container layoutManager. 
layoutManager arrangementOf: filterView put: 2. 
layoutManager arrangementOf: packagesView put: 5. 
container layout. 
sources := filterView model asSet detect: [:each | each name = 'sources'].
filterView presenter selection: sources.
!
ClassBrowserAbstract allSubclasses do: [:each | 
	(each plugins includes: DebugInfoPlugin) ifFalse: [each plugins add: DebugInfoPlugin]].
Debugger defaultExtent: 1000@600.
!
JadeLoginShell show
	position: 50@20;
	yourself.
Cursor wait showWhile: [
	Transcript flush.	"don't clear Transcript; we want to see any errors"
	SourceManager default compressSources.
	(Delay forMilliseconds: 200) wait.
	SessionManager current saveImage.
].
!
