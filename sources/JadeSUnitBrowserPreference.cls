"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeSUnitBrowserPreference
	instanceVariableNames: 'browserClass svgExternalFile isEnabled'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

