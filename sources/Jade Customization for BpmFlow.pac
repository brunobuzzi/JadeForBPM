| package |
package := Package name: 'Jade Customization for BpmFlow'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #JadeForBpmFlowSUnitBrowser;
	add: #JadeForBpmFlowSUnitPresenter;
	add: #JadeSUnitBrowserPreference;
	yourself.

package methodNames
	add: #JadeSystemBrowser -> #jadeBrowseTests;
	add: #JadeTextDocument -> #jadeBrowseTests;
	add: #JadeToolsToolBarPresenter -> #browseTests;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\Jade\Core\Object Arts\Dolphin\IDE\Base\Development System'
	'..\..\Jade\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Jade\Core\Object Arts\Dolphin\MVP\Base\Dolphin Basic Geometry'
	'..\..\Jade\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base'
	'..\..\Jade\sources\Jade System Browser'
	'..\..\Jade\sources\Jade Test Browser'
	'..\..\Jade\sources\Jade UI'
	'..\..\Jade\sources\Jade UI Base'
	'..\..\Jade\Core\Object Arts\Samples\ActiveX\Web Browser\Simple Web Browser').

package!

"Class Definitions"!

JadePreferenceObject subclass: #JadeSUnitBrowserPreference
	instanceVariableNames: 'browserClass svgExternalFile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeSUnitPresenter subclass: #JadeForBpmFlowSUnitPresenter
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

publishedAspects
	"Answer a <LookupTable> of the <Aspect>s published by the receiver."

	| aspects |
	aspects := super publishedAspects.
	aspects
		add: (Aspect choice: #browserClass from: (Array with: JadeSUnitBrowser with: JadeForBpmFlowSUnitBrowser));
		add: (Aspect string: #svgExternalFile).
	^aspects!

svgExternalFile
	^svgExternalFile!

svgExternalFile: anObject
	svgExternalFile := anObject! !
!JadeSUnitBrowserPreference categoriesFor: #browserClass!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #browserClass:!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #displayString!public! !
!JadeSUnitBrowserPreference categoriesFor: #publishedAspects!public! !
!JadeSUnitBrowserPreference categoriesFor: #svgExternalFile!accessing!private! !
!JadeSUnitBrowserPreference categoriesFor: #svgExternalFile:!accessing!private! !

JadeForBpmFlowSUnitPresenter guid: (GUID fromString: '{d65fe418-bb02-4d6a-bd3f-bb665bf4a0f6}')!
JadeForBpmFlowSUnitPresenter comment: ''!
!JadeForBpmFlowSUnitPresenter categoriesForClass!Unclassified! !
!JadeForBpmFlowSUnitPresenter methodsFor!

runSelected
	| result tests gsResult fileStream |
	result := JadeTestResult new.
	tests := Dictionary new.

	testCasesPresenter selections do: [:gsTestMethod |
			tests at: gsTestMethod className ifAbsentPut: [OrderedCollection new].
			(tests at: gsTestMethod className) add: gsTestMethod.
	].
	fileStream := FileStream write: JadeSUnitBrowserPreference default svgExternalFile text: true.
	fileStream nextPutAll: '<svg>'.
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
					svg := gciSession send:  #asSVG to: procInstance.	
					fileStream nextPutAll: svg.
				].
			].
	].
	fileStream nextPutAll: '</svg>'.
	fileStream flush; close.
	result setSummary.
	self setColorFor: result.
	WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile! !
!JadeForBpmFlowSUnitPresenter categoriesFor: #runSelected!public! !

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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 nil nil 8 #(13565952 65536) 416 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 328198 ##(Smalltalk.Point) 1601 1201 549 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() false 170 192 34 2 410 ##(Smalltalk.ReferenceView) 34 14 nil 416 34 2 8 1140850688 131073 640 nil 480 nil 7 nil nil nil 640 1180230 1 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.JadeSUnitPresenter) #resource_Default_view nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 514 1 1 514 1569 1123 640 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 16 3 0 0 49 2 0 0] 592 514 193 193 nil 27 8 'myPresenter' nil nil nil nil nil 1 nil nil nil nil 1 nil nil 738 138 144 34 3 802 #createAt:extent: 34 2 514 2719 21 514 1601 1201 416 802 #text: 34 1 8 'Jade SUnit Browser' 416 802 #updateMenuBar 592 416 882 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 79 5 0 0 10 0 0 0 111 8 0 0 98 2 0 0] 34 1 640 928 nil 27 )! !
!JadeForBpmFlowSUnitBrowser class categoriesFor: #resource_Default_view!public!resources-views! !

"Binary Globals"!

