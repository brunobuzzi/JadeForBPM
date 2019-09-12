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
					procInstance ifNotNil: [svg := gciSession send:  #asSVG to: procInstance.	
									fileStream nextPutAll: svg.
					].
				].
			].
	].
	fileStream nextPutAll: '</svg>'.
	fileStream flush; close.
	result setSummary.
	self setColorFor: result.
	"WebBrowserShell show openUrl: JadeSUnitBrowserPreference default svgExternalFile"
	ShellLibrary default shellOpen: JadeSUnitBrowserPreference default svgExternalFile! !
!JadeForBpmFlowSUnitPresenter categoriesFor: #runSelected!public! !

