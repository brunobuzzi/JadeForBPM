| package |
package := Package name: 'Jade Customization for BpmFlow'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #BpmAllSessionsPresenter;
	add: #JadeBpmTranscript;
	add: #JadeForBpmFlowSUnitBrowser;
	add: #JadeForBpmFlowSUnitPresenter;
	add: #JadeSUnitBrowserPreference;
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
	'..\Core\Object Arts\Dolphin\MVP\Views\Common Controls\Dolphin Common Controls'
	'..\Core\Object Arts\Dolphin\MVP\Views\Control Bars\Dolphin Control Bars'
	'..\Core\Object Arts\Dolphin\MVP\Models\List\Dolphin List Models'
	'..\Core\Object Arts\Dolphin\MVP\Deprecated\Dolphin MVP (Deprecated)'
	'..\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\Core\Object Arts\Dolphin\MVP\Presenters\Text\Dolphin Text Presenter'
	'..\Core\Object Arts\Dolphin\MVP\Type Converters\Dolphin Type Converters'
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

JadePreferenceObject subclass: #JadeSUnitBrowserPreference
	instanceVariableNames: 'browserClass svgExternalFile isEnabled'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
AllSessionsPresenter subclass: #BpmAllSessionsPresenter
	instanceVariableNames: ''
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

	JadeSUnitBrowserPreference default browserClass showOnSession: gciSession.
! !
!JadeSystemBrowser categoriesFor: #jadeBrowseTests!public! !

!JadeTextDocument methodsFor!

jadeBrowseTests

	JadeSUnitBrowserPreference default browserClass showOnSession: gciSession.
! !
!JadeTextDocument categoriesFor: #jadeBrowseTests!Jade!private! !

!JadeToolsToolBarPresenter methodsFor!

browseTests

	JadeSUnitBrowserPreference default browserClass showOnSession: gciSession.! !
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

JadeSUnitBrowserPreference guid: (GUID fromString: '{02446c0b-0f5b-47b9-b44a-073b71549c10}')!
JadeSUnitBrowserPreference comment: ''!
!JadeSUnitBrowserPreference categoriesForClass!Unclassified! !
!JadeSUnitBrowserPreference methodsFor!

browserClass
	^browserClass!

browserClass: anObject
	browserClass := anObject!

displayString

	^'SUnit Browser'!

initialize

	super initialize.

	isEnabled := true.!

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
		add: (Aspect boolean: #isEnabled).
	^aspects!

svgExternalFile
	^svgExternalFile!

svgExternalFile: anObject
	svgExternalFile := anObject! !
!JadeSUnitBrowserPreference categoriesFor: #browserClass!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #browserClass:!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #displayString!public! !
!JadeSUnitBrowserPreference categoriesFor: #initialize!public! !
!JadeSUnitBrowserPreference categoriesFor: #isEnabled!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #isEnabled:!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #publishedAspects!public! !
!JadeSUnitBrowserPreference categoriesFor: #svgExternalFile!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #svgExternalFile:!accessing!private! !

BpmAllSessionsPresenter guid: (GUID fromString: '{2f772bfc-1573-4dc2-89d2-08d362e9d6e8}')!
BpmAllSessionsPresenter comment: ''!
!BpmAllSessionsPresenter categoriesForClass!Unclassified! !
JadeForBpmFlowSUnitPresenter guid: (GUID fromString: '{d65fe418-bb02-4d6a-bd3f-bb665bf4a0f6}')!
JadeForBpmFlowSUnitPresenter comment: ''!
!JadeForBpmFlowSUnitPresenter categoriesForClass!Unclassified! !
!JadeForBpmFlowSUnitPresenter methodsFor!

runSelected
	| result tests gsResult fileStream isProcessSimulationTest |
	result := JadeTestResult new.
	tests := Dictionary new.

	testCasesPresenter selections do: [:gsTestMethod |
			tests at: gsTestMethod className ifAbsentPut: [OrderedCollection new].
			(tests at: gsTestMethod className) add: gsTestMethod.
	].
	fileStream := FileStream write: JadeSUnitBrowserPreference default svgExternalFile text: true.
"	fileStream nextPutAll: '<svg>'.  "
	tests keysAndValuesDo: [:className :testsToExecute  | | testInstance collectionResult procInstance svg passedSize failureSize errorSize |
			"self runTests: testsToExecute in: className result: result."
			isProcessSimulationTest := gciSession executeString: className, ' isProcessSimulationTest'.  "this is executed only for GS subclasses of <BpmProcessExecutionTest>"
			testsToExecute do: [:gsTestMethod |
				gsResult := gciSession executeString: '(', className, ' selector: #',  gsTestMethod methodName, ') run'.
				failureSize := gciSession send: #size to: (gciSession send: #failures to: gsResult).
				errorSize := gciSession send: #size to: (gciSession send: #errors to: gsResult).
				collectionResult := gciSession send: #passed to: gsResult.
				passedSize := gciSession send:  #size to: collectionResult.
				(failureSize > 0) ifTrue: [gsTestMethod result: 'failure'. result addFailure: gsTestMethod].
				(errorSize > 0) ifTrue: [gsTestMethod result: 'error'. result addError: gsTestMethod].
				(passedSize > 0) ifTrue: [gsTestMethod result: 'passed'. result addPassed: gsTestMethod].
				isProcessSimulationTest 
				ifTrue: [1 to: passedSize do: [:index | 
									testInstance := gciSession send:  #at: to: collectionResult withAll: (Array with: index).
									procInstance := gciSession send:  #procInstance to: testInstance.
									procInstance ifNotNil: [svg := gciSession send:  #asSVG to: procInstance.	
													fileStream nextPutAll: svg].
						].	
				].
			].
	].
	"fileStream nextPutAll: '</svg>'.  "
	fileStream flush; close.
	result setSummary.
	self setColorFor: result.
	"WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile"
	 (JadeSUnitBrowserPreference default isEnabled and:[isProcessSimulationTest]) ifTrue: [ShellLibrary default shellOpen: JadeSUnitBrowserPreference default svgExternalFile].!

runSelectedAndInspect
	| result tests gsResult fileStream |
	result := JadeTestResult new.
	tests := Dictionary new.

	testCasesPresenter selections do: [:gsTestMethod |
			tests at: gsTestMethod className ifAbsentPut: [OrderedCollection new].
			(tests at: gsTestMethod className) add: gsTestMethod.
	].
	fileStream := FileStream write: JadeSUnitBrowserPreference default svgExternalFile text: true.
	tests keysAndValuesDo: [:className :testsToExecute  | | testInstance collectionResult procInstance svg passedSize failureSize errorSize |
			"self runTests: testsToExecute in: className result: result."
			testsToExecute do: [:gsTestMethod |
				gsResult := gciSession executeString: '(', className, ' selector: #',  gsTestMethod methodName, ') run'.
				failureSize := gciSession send: #size to: (gciSession send: #failures to: gsResult).
				errorSize := gciSession send: #size to: (gciSession send: #errors to: gsResult).
				collectionResult := gciSession send: #passed to: gsResult.
				passedSize := gciSession send:  #size to: collectionResult.
				(failureSize > 0) ifTrue: [gsTestMethod result: 'failure'. result addFailure: gsTestMethod].
				(errorSize > 0) ifTrue: [gsTestMethod result: 'error'. result addError: gsTestMethod].
				(passedSize > 0) ifTrue: [gsTestMethod result: 'passed'. result addPassed: gsTestMethod].
				
				1 to: passedSize do: [:index | 
					testInstance := gciSession send:  #at: to: collectionResult withAll: (Array with: index).
					procInstance := gciSession send:  #procInstance to: testInstance.
					procInstance ifNotNil: [svg := gciSession send:  #asSVG to: procInstance.	
									fileStream nextPutAll: svg.
					JadeNavigationInspector showOn: procInstance session: gciSession.
					].
				].
			].
	].
	fileStream flush; close.
	result setSummary.
	self setColorFor: result.
	"WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile"
	ShellLibrary default shellOpen: JadeSUnitBrowserPreference default svgExternalFile! !
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

