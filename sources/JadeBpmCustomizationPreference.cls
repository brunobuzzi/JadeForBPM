"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeBpmCustomizationPreference
	instanceVariableNames: 'browserClass svgExternalFile isEnabled webServerCheckLoopDelay'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

