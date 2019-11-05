| package |
package := Package name: 'Jade Customization for BpmFlow'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #BpmAllSessionsPresenter;
	add: #BpmGemProcess;
	add: #JadeBpmCustomizationPreference;
	add: #JadeBpmTranscript;
	add: #JadeForBpmFlowSUnitBrowser;
	add: #JadeForBpmFlowSUnitPresenter;
	yourself.

package methodNames
	add: #JadeLoginShell -> #postLogin:;
	add: #JadeSystemBrowser -> #jadeBrowseTests;
	add: #JadeTextDocument -> #jadeBrowseTests;
	add: #JadeToolsToolBarPresenter -> #browseTests;
	add: #JadeTranscript -> #createComponents;
	add: #JadeTranscript -> #defaultAllSessionsPresenterClass;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Core\Object Arts\Dolphin\IDE\Base\Development System'
	'..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Core\Object Arts\Dolphin\MVP\Base\Dolphin Basic Geometry'
	'..\Core\Object Arts\Dolphin\MVP\Views\Cards\Dolphin Card Containers'
	'..\Core\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls'
	'..\Core\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars'
	'..\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\List\Dolphin List Presenter'
	'..\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Core\Object Arts\Dolphin\MVP\Deprecated\Dolphin MVP (Deprecated)'
	'..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter'
	'..\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters'
	'..\Core\Object Arts\Dolphin\MVP\Models\Value\Dolphin Value Models'
	'..\..\Jade\sources\Jade Inspector'
	'..\..\Jade\sources\Jade Login'
	'..\..\Jade\sources\Jade System Browser'
	'..\..\Jade\sources\Jade Test Browser'
	'..\..\Jade\sources\Jade Transcript'
	'..\..\Jade\sources\Jade UI'
	'..\..\Jade\sources\Jade UI Base'
	'..\Core\Object Arts\Dolphin\ActiveX\Shell\Windows Shell').

package!

"Class Definitions"!

Object subclass: #BpmGemProcess
	instanceVariableNames: 'name pid port gsSession pingFailed'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadePreferenceObject subclass: #JadeBpmCustomizationPreference
	instanceVariableNames: 'browserClass svgExternalFile isEnabled webServerCheckLoopDelay'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AllSessionsPresenter subclass: #BpmAllSessionsPresenter
	instanceVariableNames: 'gemsListPresenter pingLoop'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitPresenter subclass: #JadeForBpmFlowSUnitPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeTranscript subclass: #JadeBpmTranscript
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitBrowser subclass: #JadeForBpmFlowSUnitBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!JadeLoginShell methodsFor!

postLogin: aGciSession

	loginListPresenter view updateAll.
	self setWorkingDirectory.
	model 
		session: aGciSession;
		transcript: (JadeBpmTranscript showOnSession: aGciSession);
		yourself.
	aGciSession 
		when: #logout
		send: #logout:
		to: self
		with: self model.

! !
!JadeLoginShell categoriesFor: #postLogin:!public! !

!JadeSystemBrowser methodsFor!

jadeBrowseTests
	JadeBpmCustomizationPreference default browserClass showOnSession: gciSession! !
!JadeSystemBrowser categoriesFor: #jadeBrowseTests!public! !

!JadeTextDocument methodsFor!

jadeBrowseTests
	JadeBpmCustomizationPreference default browserClass showOnSession: gciSession! !
!JadeTextDocument categoriesFor: #jadeBrowseTests!Jade!private! !

!JadeToolsToolBarPresenter methodsFor!

browseTests
	JadeBpmCustomizationPreference default browserClass showOnSession: gciSession! !
!JadeToolsToolBarPresenter categoriesFor: #browseTests!public! !

!JadeTranscript methodsFor!

createComponents

	super createComponents.
	allSessionsPresenter	:= self add: self defaultAllSessionsPresenterClass	new name: 'All Sessions'.
	historyPresenter	:= self add: TextPresenter				new name: 'history'.
	mySessionPresenter	:= self add: MySessionPresenter			new name: 'My Session'.
	stoneInfoPresenter	:= self add: StoneInfoPresenter			new name: 'Stone'.
	toolbarPresenter	:= self add: JadeToolsToolBarPresenter	new name: 'Toolbar'.
	transcriptPresenter	:= self add: TranscriptPresenter			new name: 'Transcript'.
!

defaultAllSessionsPresenterClass

	^AllSessionsPresenter! !
!JadeTranscript categoriesFor: #createComponents!public! !
!JadeTranscript categoriesFor: #defaultAllSessionsPresenterClass!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

BpmGemProcess guid: (GUID fromString: '{2238a8ff-db22-4903-8be2-821ea6482294}')!
BpmGemProcess comment: ''!
!BpmGemProcess categoriesForClass!Unclassified! !
!BpmGemProcess methodsFor!

gsSession
	^gsSession!

gsSession: anObject
	gsSession := anObject!

initialize

	super initialize.

	pingFailed := false!

name
	^name!

name: anObject
	name := anObject!

pid
	^pid!

pid: anObject
	pid := anObject!

pingFailed
	^pingFailed!

pingFailed: anObject
	pingFailed := anObject!

port
	^port!

port: anObject
	port := anObject!

setPingStatusFrom: curlCommandResult
	"The receiver set the status <pingFailed> from the result <curlCommandResult> of executing a Linux 'curl' command against a Gem process"

	pingFailed := (curlCommandResult indexOfSubCollection: 'Connection refused') ~= 0

!

stopSession
	^gsSession stopSession! !
!BpmGemProcess categoriesFor: #gsSession!accessing!private! !
!BpmGemProcess categoriesFor: #gsSession:!accessing!private! !
!BpmGemProcess categoriesFor: #initialize!public! !
!BpmGemProcess categoriesFor: #name!accessing!private! !
!BpmGemProcess categoriesFor: #name:!accessing!private! !
!BpmGemProcess categoriesFor: #pid!accessing!private! !
!BpmGemProcess categoriesFor: #pid:!accessing!private! !
!BpmGemProcess categoriesFor: #pingFailed!accessing!private! !
!BpmGemProcess categoriesFor: #pingFailed:!accessing!private! !
!BpmGemProcess categoriesFor: #port!accessing!private! !
!BpmGemProcess categoriesFor: #port:!accessing!private! !
!BpmGemProcess categoriesFor: #setPingStatusFrom:!public! !
!BpmGemProcess categoriesFor: #stopSession!public! !

!BpmGemProcess class methodsFor!

new

	^super new initialize! !
!BpmGemProcess class categoriesFor: #new!public! !

JadeBpmCustomizationPreference guid: (GUID fromString: '{02446c0b-0f5b-47b9-b44a-073b71549c10}')!
JadeBpmCustomizationPreference comment: ''!
!JadeBpmCustomizationPreference categoriesForClass!Unclassified! !
!JadeBpmCustomizationPreference methodsFor!

browserClass
	^browserClass!

browserClass: anObject
	browserClass := anObject!

displayString

	^'BPM Flow Customizations'!

initialize

	super initialize.

	isEnabled := true.
	webServerCheckLoopDelay := 5.!

isEnabled
	^isEnabled!

isEnabled: anObject
	isEnabled := anObject!

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects
		add: (Aspect choice: #browserClass from: (Array with: JadeSUnitBrowser with: JadeForBpmFlowSUnitBrowser));
		add: (Aspect string: #svgExternalFile);
		add: (Aspect boolean: #isEnabled);
		add: (Aspect integer: #webServerCheckLoopDelay).
	^aspects!

svgExternalFile
	^svgExternalFile!

svgExternalFile: anObject
	svgExternalFile := anObject!

webServerCheckLoopDelay
	^webServerCheckLoopDelay!

webServerCheckLoopDelay: anObject
	webServerCheckLoopDelay := anObject! !
!JadeBpmCustomizationPreference categoriesFor: #browserClass!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #browserClass:!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #displayString!public! !
!JadeBpmCustomizationPreference categoriesFor: #initialize!public! !
!JadeBpmCustomizationPreference categoriesFor: #isEnabled!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #isEnabled:!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #publishedAspects!public! !
!JadeBpmCustomizationPreference categoriesFor: #svgExternalFile!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #svgExternalFile:!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #webServerCheckLoopDelay!accessing!private! !
!JadeBpmCustomizationPreference categoriesFor: #webServerCheckLoopDelay:!accessing!private! !

BpmAllSessionsPresenter guid: (GUID fromString: '{2f772bfc-1573-4dc2-89d2-08d362e9d6e8}')!
BpmAllSessionsPresenter comment: ''!
!BpmAllSessionsPresenter categoriesForClass!Unclassified! !
!BpmAllSessionsPresenter methodsFor!

createComponents

	super createComponents.

	gemsListPresenter := self add: ListPresenter new name: 'gemsList'.!

createSchematicWiring

	super createSchematicWiring.

	gemsListPresenter when: #selectionChanged send: #onGemProcessSelected to: self!

fillSessionList
	| portPidString portPidStringList |

	super fillSessionList.

	portPidString := gciSession executeString: 'BpmGemsInfo getGemsPidAndPortString'.

	portPidStringList := portPidString subStrings: ','.

	gemsListPresenter model: ListModel new.
	portPidStringList do: [:portPid | | portPidArray gemProc gsSession |
		portPidArray := portPid subStrings: '>>'.
		gemProc := BpmGemProcess new port: portPidArray first; pid: portPidArray second ; yourself.
		gsSession := sessionListPresenter model detect: [:each | each process printString = gemProc pid] ifNone: [].
		gsSession ifNotNil: [
			gemProc name: gsSession cacheDesc;
				gsSession: gsSession.
			gemsListPresenter model add: gemProc].
	].

	(sessionListPresenter model detect: [:gsSession | (gsSession cacheDesc = 'TimersLoop')] ifNone: [])
	ifNotNil: [:gsSession | | gemProc |
		gemProc := BpmGemProcess new name: gsSession cacheDesc; port: 'N/A'; pid: gsSession process; gsSession: gsSession; yourself.
		gemsListPresenter model add: gemProc].

	(sessionListPresenter model detect: [:gsSession | (gsSession cacheDesc = 'ScriptsLoop')] ifNone: [])
	ifNotNil: [:gsSession | | gemProc |
		gemProc := BpmGemProcess new name: gsSession cacheDesc; port: 'N/A'; pid: gsSession process; gsSession: gsSession; yourself.
		gemsListPresenter model add: gemProc].
!

gemstoneBashCurlCommandFor: aBpmGemProcess
	| command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''curl http://localhost:', aBpmGemProcess port,'/bpmflow'''.

	^command contents!

onGemProcessSelected

	gemsListPresenter hasSelection ifFalse: [^sessionListPresenter resetSelection].

	sessionListPresenter selection: gemsListPresenter selection gsSession ifAbsent: []
!

pingPort
	| result |

	gemsListPresenter hasSelection ifFalse: [^self].

	result := gciSession executeString: (self gemstoneBashCurlCommandFor: gemsListPresenter selection).

	MessageBox notify: result.

	gemsListPresenter selection setPingStatusFrom: result!

restartOnPort
	| result command |

	gemsListPresenter hasSelection ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-on.sh ', gemsListPresenter selection port, ''''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

startAll
	| result command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-all.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList
!

startOnPort
	| result command port |

	port := Prompter prompt: 'Enter the Port number:' caption: 'Jade for BpmFlow'.
	[Number fromString: port] on: Error do: [:ex | ^MessageBox notify: 'Invalid Port number [', port printString, ']' caption: 'Jade for BpmFlow'].

	 (gciSession executeString: 'BpmGemsInfo isValidPortNumber: ', port) ifFalse: [^MessageBox notify: 'Invalid Port number [', port, ']. Check file ../scripts/ports-all.ini' caption: 'Jade for BpmFlow'].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-on.sh ', port, ''''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

startPingLoop

	pingLoop := [ [true] whileTrue: [
			gemsListPresenter model do: [:bpmGemProc | | result |
				result := gciSession executeString: (self gemstoneBashCurlCommandFor: bpmGemProc).
				bpmGemProc setPingStatusFrom: result.
				(Delay forSeconds: JadeBpmCustomizationPreference default webServerCheckLoopDelay) wait.
			].
		].
	] forkAt: 4.!

startScriptsServer
	| result command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-deferred-scripts-loop.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList
!

startTimersServer
	| result command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-timers-loop.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

stopAll
	| result command |

	self stopPingLoop.

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh stop-all.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

stopOnPort
	| result command |

	gemsListPresenter hasSelection ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh stop-on.sh ', gemsListPresenter selection port, ''''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

stopPingLoop

	pingLoop ifNil: [^self].

	pingLoop terminate.

	pingLoop := nil!

stopScriptsServer
	| bpmGemProcess |

	bpmGemProcess := gemsListPresenter model detect: [:each | each name = 'ScriptsLoop'] ifNone: [^self].

	(MessageBox confirm: 'Stop Session?') ifFalse: [^self].
	bpmGemProcess stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList.!

stopTimersServer
	| bpmGemProcess |

	bpmGemProcess := gemsListPresenter model detect: [:each | each name = 'TimersLoop'] ifNone: [^self].

	(MessageBox confirm: 'Stop Session?') ifFalse: [^self].
	bpmGemProcess stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList.

	! !
!BpmAllSessionsPresenter categoriesFor: #createComponents!public! !
!BpmAllSessionsPresenter categoriesFor: #createSchematicWiring!public! !
!BpmAllSessionsPresenter categoriesFor: #fillSessionList!public!updating! !
!BpmAllSessionsPresenter categoriesFor: #gemstoneBashCurlCommandFor:!public! !
!BpmAllSessionsPresenter categoriesFor: #onGemProcessSelected!public! !
!BpmAllSessionsPresenter categoriesFor: #pingPort!public! !
!BpmAllSessionsPresenter categoriesFor: #restartOnPort!public! !
!BpmAllSessionsPresenter categoriesFor: #startAll!bpm flow commands!public! !
!BpmAllSessionsPresenter categoriesFor: #startOnPort!bpm flow commands!public! !
!BpmAllSessionsPresenter categoriesFor: #startPingLoop!bpm flow monitoring!public! !
!BpmAllSessionsPresenter categoriesFor: #startScriptsServer!bpm flow commands!public! !
!BpmAllSessionsPresenter categoriesFor: #startTimersServer!bpm flow commands!public! !
!BpmAllSessionsPresenter categoriesFor: #stopAll!bpm flow commands!public! !
!BpmAllSessionsPresenter categoriesFor: #stopOnPort!bpm flow commands!public!updating! !
!BpmAllSessionsPresenter categoriesFor: #stopPingLoop!bpm flow monitoring!public! !
!BpmAllSessionsPresenter categoriesFor: #stopScriptsServer!bpm flow commands!public! !
!BpmAllSessionsPresenter categoriesFor: #stopTimersServer!bpm flow commands!public! !

!BpmAllSessionsPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1333 21 706 1401 1001 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 154 2 0 0 10 0 0 0 86 5 0 0 254 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 816 nil nil nil 5 nil nil nil 816 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.CheckBox) 34 16 nil 816 34 2 8 1409363203 1 944 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 944 nil 8 1919058316 852486 ##(Smalltalk.NullConverter) nil nil nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 3 445 706 201 45 944 658 #text: 34 1 8 'Auto-update' 944 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 222 0 0 0 101 0 0 0 244 0 0 0] 8 #() 706 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 3 1378 #fixedViewLeft 201 1378 #fixedParentBottom -49 1378 #fixedViewTop 45 410 ##(Smalltalk.ListView) 34 30 nil 816 34 2 8 1140920397 1025 1456 590662 2 ##(Smalltalk.ListModel) 1573710 1 ##(Smalltalk.STBSortedCollectionProxy) ##(Smalltalk.SortedCollection) 560 1179910 ##(Smalltalk.IntrosortAlgorithm) 787814 3 ##(Smalltalk.BlockClosure) 2 nil 918822 ##(Smalltalk.CompiledMethod) 5 515 ##(Smalltalk.ListViewColumn) #rowSortBlock 47153027 8 #[171 221 50 6 223 4 33 112 17 106 6 89 5 90 173 221 18 233 16 252 2 0 0 4 28 0 233 50 18 143 233 32 143 150 106 233 16 252 2 0 0 4 10 0 233 50 17 143 233 33 143 150 106 60 105] #isSortable #ifTrue:ifFalse: #isSortOrderInverted #ifTrue: 1618 0 nil 1664 19 257 nil 91 513 nil 1618 0 459302 ##(Smalltalk.Context) 1 1 nil nil 1180966 ##(Smalltalk.CompiledExpression) 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each name]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #name 17 257 nil ##(Smalltalk.SortedCollection) nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fillSessionList 8 '&Update' 1 1 nil nil nil 2018 1 2050 #sendSigAbort 8 'Send Sig&Abort' 1 1 nil nil nil 2018 1 2050 #sendSigUsr1 8 'Request &Stack' 1 1 nil nil nil 2018 1 2050 #stopSession 8 'S&top Session' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1456 nil 8 1919214959 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil 9 138 144 34 21 920646 5 ##(Smalltalk.ListViewColumn) 8 '#' 61 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each id]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #id 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Serial' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each serial]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #serial 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Description' 201 #left 459270 ##(Smalltalk.Message) #cacheDesc 8 #() 2738 #<= 2768 nil nil 1456 nil 1 nil nil 2338 8 'User' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1712 nil 1456 nil 1 nil nil 2338 8 'View Age' 141 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 nil 1762 2 1 1616 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] #viewAge 2864 7 257 nil nil 1456 nil 1 nil nil 2338 8 'Oldest' 121 #center ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each hasOldestCR]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #hasOldestCR 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Gem Host' 281 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each host]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #host 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Process' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each process]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #process 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Primitive' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each primitive]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #primitive 17 257 nil nil 1456 nil 1 nil nil 2338 8 'State' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each state]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #state 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Trans' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each transaction]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #transaction 17 257 nil nil 1456 nil 1 nil nil 2338 8 'GCI IP' 241 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1618 0 1730 1 1 nil nil 1762 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each ip]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #ip 17 257 nil nil 1456 nil 1 nil nil 2338 8 'Priority' 111 #right 2738 #displayString 8 #() 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] #priority 4256 7 257 nil nil 1456 nil 1 nil nil 2338 8 'Host ID' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] #hostId 4400 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 4480 7 257 nil nil 2338 8 'Quiet' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] #quietTime 4640 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 4720 7 257 nil nil 2338 8 'Age' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] #lifeTime 4880 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 4960 7 257 nil nil 2338 8 'Backlog' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] #backlog 5120 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 5200 7 257 nil nil 2338 8 'Type' 201 #left 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 1616 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] #description 5360 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 5440 7 257 nil nil 2338 8 'Objects' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] #objects 5600 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 5680 7 257 nil nil 2338 8 'Pages' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] #pages 5840 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 5920 7 257 nil nil 2338 8 'Voting' 121 #right 2738 #displayString 4224 2738 #<= 4224 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] #voteState 6080 7 257 nil nil 1456 nil 1 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 6160 7 257 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 3 706 1401 441 1456 658 #contextMenu: 34 1 1984 1456 658 #text: 34 1 8 '#' 1456 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 189 2 0 0 221 0 0 0] 8 #() 1328 nil 27 1346 1392 3 1378 #fixedParentRight 3 1378 #fixedParentTop 3 1424 -51 410 ##(Smalltalk.PushButton) 34 20 nil 816 34 2 8 1140924416 1 6576 nil nil nil 5 nil nil nil 6576 nil 8 1919058316 2050 #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1259 443 706 141 51 6576 658 #isEnabled: 8 #(false) 6576 658 #text: 34 1 8 'Update' 6576 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 2 0 0 221 0 0 0 187 2 0 0 246 0 0 0] 8 #() 1328 nil 29 1346 1378 #fixedViewRight -139 6544 -1 1424 -51 1440 51 170 192 34 4 944 8 'autoUpdate' 1456 8 'sessionList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 1401 495 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 247 0 0 0] 34 3 1456 6576 944 1328 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 7184 nil 1936 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 7184 nil 1 #left nil nil nil 706 1 1 706 9 9 nil 7280 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 495 706 1401 11 7184 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 247 0 0 0 188 2 0 0 252 0 0 0] 8 #() 1328 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 7472 nil nil nil 5 nil nil nil 7472 514 170 176 560 false 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 505 706 1401 497 7472 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 252 0 0 0 188 2 0 0 244 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 7472 34 2 8 1140850688 131073 7744 nil nil nil 5 nil nil nil 7744 882 170 176 34 4 410 ##(Smalltalk.GroupBox) 34 14 nil 7744 34 2 8 1140850695 65 7856 nil 590086 ##(Smalltalk.ColorNone) 8 4294967295 nil 5 nil nil nil 7856 nil 8 1919058316 594 138 144 34 2 658 #createAt:extent: 34 2 706 11 7 706 675 481 7856 658 #text: 34 1 8 'Running Web Servers' 7856 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 3 0 0 0 86 1 0 0 243 0 0 0] 8 #() 1328 nil 27 1346 1392 11 6544 -9 6560 7 1424 -9 410 ##(Smalltalk.ListView) 34 30 nil 7744 34 2 8 1409355853 1025 8208 1522 138 144 560 nil 1904 1936 nil 5 1970 nil true 34 5 2018 1 2050 #pingPort 8 'Ping Port' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2018 1 2050 #restartOnPort 8 'Restart Web Server' 1 1 nil nil nil 2018 1 2050 #startOnPort 8 'Start Web Server on' 1 1 nil nil nil 2018 1 2050 #stopOnPort 8 'Stop Web Server' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 8208 nil 8 1919214959 2738 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 2288 nil nil nil nil nil nil 138 144 34 3 2338 8 'Name' 501 #left 2738 #name 8 #() 1618 0 nil 1762 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:a :b | a name <= b name]' 8 #[30 105 226 0 226 32 130 106] #name 8720 7 513 nil nil nil 8208 nil 1 nil 1618 0 nil 1762 8 1 ##(Smalltalk.BpmGemProcess) 8 'doIt' 8 '[:ctx | ctx item pingFailed ifTrue: [ctx forecolor: Color red] ifFalse: [ctx forecolor: Color black]]' 8 #[36 105 226 0 159 122 17 47 161 180 106 17 47 164 180 106] #item #pingFailed 983558 ##(Smalltalk.VariableBinding) #Color ##(Smalltalk.Color) #red #forecolor: #ifTrue:ifFalse: #black 8800 7 257 nil 2338 8 'PID' 301 #left 2738 #pid 8704 1618 0 nil 1762 2 1 1616 8 'doIt' 8 '[:a :b | a pid <= b pid]' 8 #[30 105 226 0 226 32 130 106] #pid 8960 7 513 nil nil nil 8208 nil 1 nil nil 2338 8 'Port' 301 #left 2738 #port 8704 1618 0 nil 1762 2 1 1616 8 'doIt' 8 '[:a :b | a port <= b port]' 8 #[30 105 226 0 226 32 130 106] #port 9088 7 513 nil nil nil 8208 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 31 51 706 635 417 8208 658 #contextMenu: 34 1 8304 8208 658 #text: 34 1 8 'Name' 8208 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 25 0 0 0 76 1 0 0 233 0 0 0] 8 #() 1328 nil 27 1346 1392 31 6544 -29 6560 51 1424 -29 170 192 34 2 8208 8 'gemsList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 695 497 7744 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 91 1 0 0 248 0 0 0] 34 2 8208 7856 1328 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 7472 34 2 8 1140850688 1 9664 nil 1936 nil 517 nil nil nil 7250 9664 nil 1 #left nil nil nil 7280 7296 nil 7280 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 695 1 706 11 497 9664 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 1 0 0 0 0 0 0 96 1 0 0 248 0 0 0] 8 #() 1328 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7472 34 2 8 1140850688 131073 9904 nil nil nil 5 nil nil nil 9904 nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 705 1 706 697 497 9904 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 96 1 0 0 0 0 0 0 188 2 0 0 248 0 0 0] 34 8 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 10144 nil nil nil 5 nil nil nil 10144 nil 8 1919058316 2050 #startAll 8 'Start All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 21 706 141 115 10144 658 #isEnabled: 8 #(false) 10144 658 #text: 34 1 8 'Start All Web Servers' 10144 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 10 0 0 0 71 0 0 0 67 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 10496 nil nil nil 5 nil nil nil 10496 nil 8 1919058316 2050 #stopAll 8 'Stop All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 139 706 141 119 10496 658 #isEnabled: 8 #(false) 10496 658 #text: 34 1 8 'Stop All Web Servers' 10496 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 69 0 0 0 71 0 0 0 128 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 10848 nil nil nil 5 nil nil nil 10848 nil 8 1919058316 2050 #startPingLoop 8 'Start Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 281 706 145 81 10848 658 #isEnabled: 8 #(false) 10848 658 #text: 34 1 8 'Start Ping Loop' 10848 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 140 0 0 0 73 0 0 0 180 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 11200 nil nil nil 5 nil nil nil 11200 nil 8 1919058316 2050 #stopPingLoop 8 'Stop Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 369 706 145 81 11200 658 #isEnabled: 8 #(false) 11200 658 #text: 34 1 8 'Stop Ping Loop' 11200 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 184 0 0 0 73 0 0 0 224 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 11552 nil nil nil 5 nil nil nil 11552 nil 8 1919058316 2050 #startTimersServer 8 'Start Timers Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 161 21 706 141 115 11552 658 #isEnabled: 8 #(false) 11552 658 #text: 34 1 8 'Start Timers Server' 11552 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 10 0 0 0 150 0 0 0 67 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 11904 nil nil nil 5 nil nil nil 11904 nil 8 1919058316 2050 #stopTimersServer 8 'Stop Timers Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 161 139 706 141 115 11904 658 #isEnabled: 8 #(false) 11904 658 #text: 34 1 8 'Stop Timers Server' 11904 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 69 0 0 0 150 0 0 0 126 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 12256 nil nil nil 5 nil nil nil 12256 nil 8 1919058316 2050 #startScriptsServer 8 'Start Scripts Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 319 21 706 141 115 12256 658 #isEnabled: 8 #(false) 12256 658 #text: 34 1 8 'Start Scripts Server' 12256 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 0 0 0 10 0 0 0 229 0 0 0 67 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9904 34 2 8 1140924416 1 12608 nil nil nil 5 nil nil nil 12608 nil 8 1919058316 2050 #stopScriptsServer 8 'Stop Scripts Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 319 139 706 141 115 12608 658 #isEnabled: 8 #(false) 12608 658 #text: 34 1 8 'Stop Scripts Server' 12608 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 0 0 0 69 0 0 0 229 0 0 0 126 0 0 0] 8 #() 1328 nil 29 1328 nil 27 1328 nil 27 1328 nil 27 )! !
!BpmAllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeForBpmFlowSUnitPresenter guid: (GUID fromString: '{d65fe418-bb02-4d6a-bd3f-bb665bf4a0f6}')!
JadeForBpmFlowSUnitPresenter comment: ''!
!JadeForBpmFlowSUnitPresenter categoriesForClass!Unclassified! !
!JadeForBpmFlowSUnitPresenter methodsFor!

runSelected
	| result tests gsResult fileStream isProcessSimulationTest |
	result := JadeTestResult new.
	tests := Dictionary new.
	testCasesPresenter selections do: 
			[:gsTestMethod |
			tests at: gsTestMethod className ifAbsentPut: [OrderedCollection new].
			(tests at: gsTestMethod className) add: gsTestMethod].
	fileStream := FileStream write: JadeBpmCustomizationPreference default svgExternalFile text: true.
	"	fileStream nextPutAll: '<svg>'.  "
	tests keysAndValuesDo: 
			[:className :testsToExecute |
			| testInstance collectionResult procInstance svg passedSize failureSize errorSize |
			"self runTests: testsToExecute in: className result: result."
			isProcessSimulationTest := gciSession executeString: className , ' isProcessSimulationTest'.	"this is executed only for GS subclasses of <BpmProcessExecutionTest>"
			testsToExecute do: 
					[:gsTestMethod |
					gsResult := gciSession
								executeString: '(' , className , ' selector: #' , gsTestMethod methodName , ') run'.
					failureSize := gciSession send: #size to: (gciSession send: #failures to: gsResult).
					errorSize := gciSession send: #size to: (gciSession send: #errors to: gsResult).
					collectionResult := gciSession send: #passed to: gsResult.
					passedSize := gciSession send: #size to: collectionResult.
					failureSize > 0
						ifTrue: 
							[gsTestMethod result: 'failure'.
							result addFailure: gsTestMethod].
					errorSize > 0
						ifTrue: 
							[gsTestMethod result: 'error'.
							result addError: gsTestMethod].
					passedSize > 0
						ifTrue: 
							[gsTestMethod result: 'passed'.
							result addPassed: gsTestMethod].
					isProcessSimulationTest
						ifTrue: 
							[1 to: passedSize
								do: 
									[:index |
									testInstance := gciSession
												send: #at:
												to: collectionResult
												withAll: (Array with: index).
									procInstance := gciSession send: #procInstance to: testInstance.
									procInstance
										ifNotNil: 
											[svg := gciSession send: #asSVG to: procInstance.
											fileStream nextPutAll: svg]]]]].
	"fileStream nextPutAll: '</svg>'.  "
	fileStream
		flush;
		close.
	result setSummary.
	self setColorFor: result.
	"WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile"
	(JadeBpmCustomizationPreference default isEnabled and: [isProcessSimulationTest])
		ifTrue: [ShellLibrary default shellOpen: JadeBpmCustomizationPreference default svgExternalFile]!

runSelectedAndInspect
	| result tests gsResult fileStream |
	result := JadeTestResult new.
	tests := Dictionary new.
	testCasesPresenter selections do: 
			[:gsTestMethod |
			tests at: gsTestMethod className ifAbsentPut: [OrderedCollection new].
			(tests at: gsTestMethod className) add: gsTestMethod].
	fileStream := FileStream write: JadeBpmCustomizationPreference default svgExternalFile text: true.
	tests keysAndValuesDo: 
			[:className :testsToExecute |
			| testInstance collectionResult procInstance svg passedSize failureSize errorSize |
			"self runTests: testsToExecute in: className result: result."
			testsToExecute do: 
					[:gsTestMethod |
					gsResult := gciSession
								executeString: '(' , className , ' selector: #' , gsTestMethod methodName , ') run'.
					failureSize := gciSession send: #size to: (gciSession send: #failures to: gsResult).
					errorSize := gciSession send: #size to: (gciSession send: #errors to: gsResult).
					collectionResult := gciSession send: #passed to: gsResult.
					passedSize := gciSession send: #size to: collectionResult.
					failureSize > 0
						ifTrue: 
							[gsTestMethod result: 'failure'.
							result addFailure: gsTestMethod].
					errorSize > 0
						ifTrue: 
							[gsTestMethod result: 'error'.
							result addError: gsTestMethod].
					passedSize > 0
						ifTrue: 
							[gsTestMethod result: 'passed'.
							result addPassed: gsTestMethod].
					1 to: passedSize
						do: 
							[:index |
							testInstance := gciSession
										send: #at:
										to: collectionResult
										withAll: (Array with: index).
							procInstance := gciSession send: #procInstance to: testInstance.
							procInstance
								ifNotNil: 
									[svg := gciSession send: #asSVG to: procInstance.
									fileStream nextPutAll: svg.
									JadeNavigationInspector showOn: procInstance session: gciSession]]]].
	fileStream
		flush;
		close.
	result setSummary.
	self setColorFor: result.
	"WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile"
	ShellLibrary default shellOpen: JadeBpmCustomizationPreference default svgExternalFile! !
!JadeForBpmFlowSUnitPresenter categoriesFor: #runSelected!public! !
!JadeForBpmFlowSUnitPresenter categoriesFor: #runSelectedAndInspect!public! !

!JadeForBpmFlowSUnitPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil nil nil 5 nil nil nil 416 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 512 nil nil nil 5 nil nil nil 512 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 34 2 410 ##(Smalltalk.StaticText) 34 16 nil 512 34 2 8 1140850944 1 672 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil 263174 ##(Smalltalk.Font) nil true 459014 ##(Smalltalk.LOGFONT) 8 #[240 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 188 2 0 0 0 0 0 0 1 2 1 34 83 121 115 116 101 109 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 328198 ##(Smalltalk.Point) 193 193 nil 672 nil 8 1918971218 852486 ##(Smalltalk.NullConverter) nil nil nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 850 1 55 850 1241 57 672 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 108 2 0 0 55 0 0 0] 8 #() 850 193 193 nil 27 8 'textResult' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 111 512 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 55 0 0 0] 34 2 410 ##(Smalltalk.ContainerView) 34 15 nil 512 34 2 8 1140850688 131073 1328 nil 752 nil 5 nil nil nil 1328 482 1 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 1408 nil nil nil 5 nil nil nil 1408 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1021 1 850 221 55 1408 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 1 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 1408 34 2 8 1140853580 131137 1680 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 517 nil nil nil 1680 1760 8 1919160497 nil 170 192 34 8 10053 1246790 1 ##(Smalltalk.ToolbarBitmapButton) 1680 1 1180998 4 ##(Smalltalk.CommandDescription) #runAll 8 'Run All' 1 1 nil 10053 395334 3 ##(Smalltalk.Bitmap) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'Tools.bmp' 2032142 ##(Smalltalk.STBExternalResourceLibraryProxy) 8 'dolphindr7.dll' nil nil 7 850 1857 33 51 10059 1246790 1 ##(Smalltalk.ToolbarSystemButton) 1680 1 1874 #reset 8 'Reset' 1 1 nil 10059 1 9 10057 1842 1680 1 1874 #debug 8 'Debug' 1 1 nil 10057 1936 15 10055 1842 1680 1 1874 #runSelected 8 'Run Selected' 1 1 nil 10055 1936 83 34 6 1856 2176 1049158 1 ##(Smalltalk.ToolbarSeparator) 1680 1 2128 2242 1680 1 2080 nil nil 1 nil 850 33 33 850 45 45 nil nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 221 55 1680 994 #updateSizePosted 624 1680 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 110 0 0 0 27 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 1328 34 2 8 1140850688 131073 2496 nil 752 nil 5 nil nil nil 2496 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1021 55 2496 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.Toolbar) 34 25 nil 2496 34 2 8 1409288972 131137 2768 nil 1760 nil 517 nil 770 nil true 802 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0] 850 193 193 nil 2768 1760 8 1919160497 170 192 624 170 192 34 4 10051 1842 2768 1 1874 #commitTransaction 8 'Commit Transaction' 1 1 nil 10051 1936 27 10049 1842 2768 1 1874 #abortTransaction 8 'Abort Transaction' 1 1 nil 10049 1936 1 34 2 3008 2960 nil nil 1 nil 850 33 33 850 45 45 nil 656198 1 ##(Smalltalk.FlowLayout) 1 1 1 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 1021 55 2768 994 #updateSizePosted 624 2768 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 254 1 0 0 27 0 0 0] 8 #() 1136 nil 27 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 55 1328 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 27 0 0 0] 34 2 2496 1408 1136 nil 27 672 1136 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 3488 nil 752 nil 5 nil nil nil 3488 578 170 176 624 false 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 811 850 1241 61 3488 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 149 1 0 0 108 2 0 0 179 1 0 0] 34 1 410 ##(Smalltalk.StatusBar) 34 18 nil 3488 34 2 8 1409288460 1 3760 nil 1760 nil 5 nil 770 nil true 802 8 #[243 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 144 1 0 0 0 0 0 0 3 2 1 34 65 114 105 97 108 0 159 4 0 134 63 1 0 0 204 53 63 1 2 0 20 59 0 0 0 0 247 0 5 86 111 1] 850 193 193 nil 3760 nil 8 1918999672 170 192 34 8 853766 ##(Smalltalk.StatusBarItem) 1 401 3760 nil 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each testCasesSize]' 8 #[30 105 226 0 106] #testCasesSize 3984 7 257 nil 3970 0 nil 918822 ##(Smalltalk.CompiledMethod) 2 3 3936 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] #iconImageIndex 4080 7 257 nil nil 8 'statusTestCases' 3938 1 401 3760 nil 3970 0 nil 4002 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each testClassesSize]' 8 #[30 105 226 0 106] #testClassesSize 4176 7 257 nil 4080 nil 8 'statusTestsClasses' 3938 1 401 3760 nil 3970 0 nil 4002 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each getTestCounter]' 8 #[30 105 226 0 106] #getTestCounter 4288 7 257 nil 3970 0 nil 4098 2 3 3936 #defaultGetImageBlock 405994851 8 #[30 105 226 0 106] #iconImageIndex 4368 7 257 nil nil 8 'statusTestCounter' 3938 1 401 3760 nil 3970 0 nil 4002 2 1 ##(Smalltalk.Message) 8 'doIt' 8 '[:each | each packagesSize]' 8 #[30 105 226 0 106] #packagesSize 4448 7 257 nil 459270 ##(Smalltalk.Message) #iconImageIndex 8 #() 1049926 1 ##(Smalltalk.IconImageManager) 8 'statusPackages' 34 4 4432 4160 3952 4272 1115142 ##(Smalltalk.StatusBarNullItem) 513 1 3760 nil 578 170 176 624 false 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 1 850 1241 61 3760 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 108 2 0 0 30 0 0 0] 8 #() 1136 nil 27 1136 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 4864 nil nil nil 5 nil nil nil 4864 578 170 176 624 false 170 192 34 6 410 ##(Smalltalk.ListView) 34 30 nil 4864 34 2 8 1409355849 1025 4992 590662 2 ##(Smalltalk.ListModel) 138 144 624 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 1760 nil 5 nil nil nil 4992 nil 8 1919214959 4530 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4592 nil nil nil nil nil nil 138 144 34 1 920646 5 ##(Smalltalk.ListViewColumn) 8 'GemStone Packages' 399 #left 4530 #displayString 5168 ##(Smalltalk.SortedCollection) nil nil 4992 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 1 1 850 407 701 4992 994 #text: 34 1 8 'GemStone Packages' 4992 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 203 0 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'packages' 410 ##(Smalltalk.MultipleSelectionListView) 34 30 nil 4864 34 2 8 1140920393 1025 5536 5058 138 144 624 nil 5120 1760 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 7 984134 2 ##(Smalltalk.CommandMenuItem) 1 1874 #runAll 8 'Run All' 9347 1 nil nil nil 5682 1 1874 #runSelected 8 'Run Selected' 9383 1 nil nil nil 5682 1 1874 #runSelectedAndInspect 8 'Run Selected and Inspect' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 5682 1 1874 #debug 8 'Debug' 9349 1 nil nil nil 5842 4097 5682 1 1874 #reset 8 'Reset' 9381 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 5536 nil 8 1919214959 4530 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4592 nil nil nil nil nil nil 138 144 34 1 5218 8 'Test Cases' 401 #left 4530 #methodName 8 #() 3970 0 nil 4002 2 1 106 8 'Dolphin' 8 'SortedCollection' 8 'doIt' 8 '[:a :b | a methodName <= b methodName]' 8 #[30 105 226 0 226 32 130 106] #methodName 6144 7 513 nil nil nil 5536 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 3 994 #createAt:extent: 34 2 850 833 1 850 409 701 5536 994 #contextMenu: 34 1 5648 5536 994 #text: 34 1 8 'Test Cases' 5536 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 160 1 0 0 0 0 0 0 108 2 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'testCases' 410 ##(Smalltalk.ListView) 34 30 nil 4864 34 2 8 1409355849 1025 6560 5058 138 144 624 nil 5120 1760 nil 5 nil nil nil 6560 nil 8 1919214959 4530 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 4592 nil nil nil nil nil nil 138 144 34 1 5218 8 'Test Classes' 399 #left 4530 #displayString 6688 ##(Smalltalk.SortedCollection) nil nil 6560 nil 3 nil nil #report 624 nil 131169 nil 34 4 nil nil 850 1 1 nil 930 138 144 34 2 994 #createAt:extent: 34 2 850 417 1 850 407 701 6560 994 #text: 34 1 8 'Test Classes' 6560 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 208 0 0 0 0 0 0 0 155 1 0 0 94 1 0 0] 8 #() 1136 nil 27 8 'testClasses' nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1 111 850 1241 701 4864 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 108 2 0 0 149 1 0 0] 34 5 4992 410 ##(Smalltalk.Splitter) 34 12 nil 4864 34 2 8 1140850688 1 7200 nil 1760 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 7200 nil 1 #left nil nil nil 6288 850 9 9 nil 6288 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 407 1 850 11 701 7200 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 203 0 0 0 0 0 0 0 208 0 0 0 94 1 0 0] 8 #() 1136 nil 27 6560 410 ##(Smalltalk.Splitter) 34 12 nil 4864 34 2 8 1140850688 1 7472 nil 1760 nil 517 nil nil nil 7266 7472 nil 1 #left nil nil nil 6288 7296 nil 6288 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 823 1 850 11 701 7472 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 155 1 0 0 0 0 0 0 160 1 0 0 94 1 0 0] 8 #() 1136 nil 27 5536 1136 nil 27 170 192 624 nil 930 138 144 34 1 994 #createAt:extent: 34 2 850 1493 21 850 1241 871 416 1074 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 234 2 0 0 10 0 0 0 86 5 0 0 189 1 0 0] 34 3 512 4864 3488 1136 nil 27 )! !
!JadeForBpmFlowSUnitPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

JadeBpmTranscript guid: (GUID fromString: '{7fcd3b1f-c6c9-449f-8d07-02cf7f930d4b}')!
JadeBpmTranscript comment: ''!
!JadeBpmTranscript categoriesForClass!Unclassified! !
!JadeBpmTranscript methodsFor!

defaultAllSessionsPresenterClass

	^BpmAllSessionsPresenter! !
!JadeBpmTranscript categoriesFor: #defaultAllSessionsPresenterClass!public! !

!JadeBpmTranscript class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1201 801 549 nil nil nil 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 4 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1140850688 131073 608 nil 480 nil 7 nil nil nil 608 655878 ##(Smalltalk.CardLayout) 138 144 34 6 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'Transcript' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 16 8 'doIt' 8 'CardLabel text: ''Transcript'' iconBlock: [Icon fromId: ''TranscriptShell.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 983558 ##(Smalltalk.VariableBinding) #CardLabel 768 800 930 #Icon ##(Smalltalk.Icon) 8 'TranscriptShell.ico' #fromId: #text:iconBlock: 832 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140916224 131073 992 nil 480 nil 5 nil nil nil 992 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 9 53 514 1153 573 992 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 514 193 193 nil 27 738 770 8 'History' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''History'' iconBlock: [Icon fromId: ''MethodHistory.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 1344 960 8 'MethodHistory.ico' #fromId: #text:iconBlock: 1360 11 1 nil nil 410 ##(Smalltalk.MultilineTextEdit) 34 16 nil 608 34 2 8 1143017796 1025 1456 nil 480 nil 5 nil nil nil 1456 nil 8 1919195819 852486 ##(Smalltalk.NullConverter) nil nil 11 1090 138 144 34 2 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 1456 1154 #setMarginWidths: 34 1 8 #(3 3) 1456 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 8 #() 1296 nil 27 738 770 8 'Stone' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''Stone'' iconBlock: [Icon fromFile: ''icons\GS32.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 1808 960 8 'icons\GS32.ico' #fromFile: #text:iconBlock: 1824 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 1920 nil nil nil 5 nil nil nil 1920 1058 ##(Smalltalk.StoneInfoPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 1920 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 738 770 8 'My Session' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''My Session'' iconBlock: [Icon fromId: ''SMALLTALKOPTIONSFOLDER.ICO'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 2176 960 8 'SMALLTALKOPTIONSFOLDER.ICO' #fromId: #text:iconBlock: 2192 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 2288 nil nil nil 5 nil nil nil 2288 1058 ##(Smalltalk.MySessionPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 2288 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 738 770 8 'Workspace' 818 0 nil 850 7 1 16 8 'doIt' 8 'CardLabel text: ''Workspace'' iconBlock: [Icon fromId: ''SmalltalkWorkspace.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 944 2544 960 8 'SmalltalkWorkspace.ico' #fromId: #text:iconBlock: 2560 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 2656 nil 480 nil 5 nil nil nil 2656 1058 ##(Smalltalk.TranscriptPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 2656 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 738 770 8 'All Sessions' 818 0 nil 850 7 1 ##(Smalltalk.SmallInteger) 8 'doIt' 8 '(CardLabel text: ''All Sessions'' iconBlock: [Icon fromId: ''ListView.ico''])' 8 #[45 30 35 113 47 32 180 106 195 105] 930 #CardLabel 768 2912 930 #Icon ##(Smalltalk.Icon) 8 'ListView.ico' #fromId: #text:iconBlock: 2928 11 1 nil nil 410 ##(Smalltalk.ReferenceView) 34 14 nil 608 34 2 8 1140850688 131073 3056 nil 480 nil 7 nil nil nil 3056 1058 ##(Smalltalk.BpmAllSessionsPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 9 53 514 1153 573 3056 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 68 2 0 0 56 1 0 0] 1280 1296 nil 27 3056 170 192 34 12 992 8 'Transcript' 1920 8 'Stone' 3056 8 'All Sessions' 1456 8 'history' 2656 8 'codePane' 2288 8 'My Session' nil 410 ##(Smalltalk.TabView) 34 23 nil 608 34 2 8 1140916736 1 3408 590662 2 ##(Smalltalk.ListModel) 138 144 34 6 784 1328 1792 2160 2896 2528 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 721158 ##(Smalltalk.SystemColor) 31 nil 1 nil nil nil 3408 nil 8 1918885598 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil #smallIcons 1090 138 144 34 3 1154 #createAt:extent: 34 2 514 1 1 514 1169 633 3408 1154 #basicSelectionsByIndex: 34 1 8 #(5) 3408 1154 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 3408 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 60 1 0 0] 8 #() 1296 nil 27 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 1 51 514 1169 633 608 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 25 0 0 0 72 2 0 0 85 1 0 0] 34 7 992 1456 1920 2288 3056 2656 3408 1296 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 4082 #fixedParentRight 1 4082 #fixedParentTop 51 4082 #fixedParentBottom 1 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 4160 nil nil nil 7 nil nil nil 4160 1058 ##(Smalltalk.JadeToolsToolBarPresenter) #resource_Default_view nil 1090 138 144 34 1 1154 #createAt:extent: 34 2 514 1 1 514 1169 51 4160 1234 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 72 2 0 0 25 0 0 0] 1280 1296 nil 27 4050 4096 1 4112 1 4128 1 4082 #fixedViewTop 51 170 192 34 4 608 8 'cardContainer' 4160 8 'Toolbar' nil 461638 4 ##(Smalltalk.MenuBar) nil true 34 5 265030 4 ##(Smalltalk.Menu) nil true 34 9 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fileNew 8 '&New Workspace' 9373 1 nil nil nil 4578 1 4610 #fileOpen 8 '&Open Workspace...' 9375 1 nil nil nil 4578 1 4610 #fileSave 8 '&Save' 9383 1 nil nil nil 4578 1 4610 #fileSaveAs 8 'Save &As...' 1 1 nil nil nil 4578 1 4610 #fileRevert 8 '&Revert' 1025 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 4578 1 4610 #resetCursor 8 'Reset &Cursor' 1025 1 nil nil nil 4850 4097 4578 1 4610 #exit 8 'E&xit Jade' 17639 1 nil nil nil 8 '&File' nil 1 nil nil 56833 nil nil 4530 nil true 34 15 4578 1 4610 #undo 8 '&Undo' 9397 1 nil nil nil 4578 1 4610 #redo 8 'R&edo' 9395 1 nil nil nil 4850 4097 4578 1 4610 #editCut 8 'Cu&t' 9393 1 nil nil nil 4578 1 4610 #editCopy 8 '&Copy' 9351 1 nil nil nil 4578 1 4610 #editPaste 8 '&Paste' 9389 1 nil nil nil 4578 1 4610 #editSelectAll 8 'Select &All' 9347 1 nil nil nil 4578 1 4610 #editDelete 8 '&Delete' 1629 1 nil nil nil 4850 4097 4578 1 4610 #editFind 8 '&Find...' 9357 1 nil nil nil 4578 1 4610 #editFindNext 8 'Find &Next' 1253 1 nil nil nil 4578 1 4610 #editReplace 8 '&Replace...' 9361 1 nil nil nil 4850 4097 4578 1 4610 #addQuotesToSelection 8 'Add &Quotes' 1 1 nil nil nil 4578 1 4610 #removeQuotesFromSelection 8 'Re&move Quotes' 1 1 nil nil nil 8 '&Edit' nil 1 nil nil 56859 nil nil 4530 nil true 34 10 4578 1 4610 #jadeBrowseClasses 8 '&Browse Classes' 9349 1 nil nil nil 4578 1 4610 #browseMonticelloRepositories 8 'Browser &Monticello Repositories' 9371 1 nil nil nil 4578 1 4610 #jadeBrowseObjectLog 8 'Browse &Object Log' 9369 1 nil nil nil 4578 1 4610 #browseProcesses 8 'Browse &Processes' 9377 1 nil nil nil 4578 1 4610 #jadeBrowseTests 8 'Browse SUnit &Tests' 9385 1 nil nil nil 4578 1 4610 #autocompletion 8 'Configure Autocompletion' 1 1 nil nil nil 4578 1 4610 #profStef 8 'Prof&Stef Tutorial' 1 1 nil nil nil 4850 4097 4578 1 4610 #inspectOop 8 'Inspect Oop ...' 1 1 nil nil nil 4578 1 4610 #describeErrorNumber 8 'Description for Error Number ...' 1025 1 nil nil nil 8 '&Browse' nil 134217729 nil nil 56879 nil nil 4530 nil true 34 13 4578 1 4610 #stopHeartbeat 8 'Stop Idle Session Check' 1 1 nil nil nil 4578 1 4610 #sleepAndCommit 8 '&Sleep and Commit' 1 1 nil nil nil 4578 1 4610 #abortTransaction 8 '&Abort Transaction' 1 1 nil nil nil 4578 1 4610 #commitTransaction 8 '&Commit Transaction' 1 1 nil nil nil 4850 4097 4578 1 4610 #jadeInspect 8 '&Inspect' 9379 1 nil nil nil 4578 1 4610 #jadeDisplay 8 '&Display' 9353 1 nil nil nil 4578 1 4610 #jadeExecute 8 '&Execute' 9355 1 nil nil nil 4578 1 4610 #jadeDebug 8 'Debu&g' 9359 1 nil nil nil 4578 1 4610 #fileIn 8 'Fi&le In' 1 1 nil nil nil 4850 4097 4578 1 4610 #fileOutJadeServer 8 '&File Out JadeServer' 1 1 nil nil nil 4578 1 4610 #addJadeServerToUserGlobals 8 'Add JadeServer to UserGlobals' 1 1 nil nil nil 8 '&Jade' nil 1 nil nil 56903 nil nil 4530 nil true 34 1 4578 1 4610 #aboutJade 8 '&About Jade' 1 1 nil nil nil 8 '&Help' nil 1 nil nil 56907 nil nil 8 '' nil 1 nil nil nil nil nil nil nil nil 1 263494 3 ##(Smalltalk.Icon) nil true 1572870 ##(Smalltalk.ImageRelativeFileLocator) 8 'icons\GS32.ico' nil 6898 nil true 6944 8 'icons\GS16.ico' nil nil nil 1 nil nil 1090 138 144 34 3 1154 #createAt:extent: 34 2 514 3059 21 514 1201 801 416 1154 #text: 34 1 8 'Jade Session' 416 1154 #updateMenuBar 1280 416 1234 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 81 8 0 0 154 1 0 0] 34 2 4160 608 1296 nil 27 )! !
!JadeBpmTranscript class categoriesFor: #resource_Default_view!public!resources-views! !

JadeForBpmFlowSUnitBrowser guid: (GUID fromString: '{4f26cd89-7417-43ca-b7ac-1ede3b3c09c7}')!
JadeForBpmFlowSUnitBrowser comment: ''!
!JadeForBpmFlowSUnitBrowser categoriesForClass!Unclassified! !
!JadeForBpmFlowSUnitBrowser methodsFor!

presenterClass

	^JadeForBpmFlowSUnitPresenter! !
!JadeForBpmFlowSUnitBrowser categoriesFor: #presenterClass!private! !

!JadeForBpmFlowSUnitBrowser class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 640 nil 480 nil 7 nil nil nil 640 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeForBpmFlowSUnitPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1569 1123 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 49 2 0 0] 592 514 193 193 nil 27 8 'myPresenter' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 738 138 144 34 3 802 #createAt:extent: 34 2 514 3059 21 514 1601 1201 416 802 #text: 34 1 8 'Jade SUnit Browser' 416 802 #updateMenuBar 592 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 5 0 0 10 0 0 0 25 9 0 0 98 2 0 0] 34 1 640 928 nil 27 )! !
!JadeForBpmFlowSUnitBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

