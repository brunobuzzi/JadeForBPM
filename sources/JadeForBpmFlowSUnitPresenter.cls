"Filed out from Dolphin Smalltalk 7"!

JadeSUnitPresenter subclass: #JadeForBpmFlowSUnitPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JadeForBpmFlowSUnitPresenter guid: (GUID fromString: '{d65fe418-bb02-4d6a-bd3f-bb665bf4a0f6}')!
JadeForBpmFlowSUnitPresenter comment: ''!
!JadeForBpmFlowSUnitPresenter categoriesForClass!Unclassified! !
!JadeForBpmFlowSUnitPresenter methodsFor!

runSelected
	| result tests gsResult fileStream |
	result := JadeTestResult new.
	tests := Dictionary new.

	testCasesPresenter selections do: 
			[:gsTestMethod |
			tests at: gsTestMethod className ifAbsentPut: [OrderedCollection new].
			(tests at: gsTestMethod className) add: gsTestMethod].
	fileStream := FileStream write: JadeSUnitBrowserPreference default svgExternalFile text: true.
	fileStream nextPutAll: '<svg>'.
	tests keysAndValuesDo: [:className :testsToExecute  | | testInstance collectionResult procInstance svg size |
			"self runTests: testsToExecute in: className result: result."
			testsToExecute do: [:gsTestMethod |

				gsResult := gciSession executeString: '(', className, ' selector: #',  gsTestMethod methodName, ') run'.
				collectionResult := gciSession send: #passed to: gsResult.
				size := gciSession send:  #size to: collectionResult.
				1 to: size do: [:index | 
					testInstance := gciSession send:  #at: to: collectionResult withAll: (Array with: index).
					procInstance := gciSession send:  #procInstance to: testInstance.
					svg := gciSession send:  #asSVG to: procInstance.	
					fileStream nextPutAll: svg.
				].
			].
	].
	fileStream nextPutAll: '</svg>'.
	fileStream flush; close.
	WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile! !
!JadeForBpmFlowSUnitPresenter categoriesFor: #runSelected!public! !

