"Filed out from Dolphin Smalltalk 7"!

JadePreferenceObject subclass: #JadeSUnitBrowserPreference
	instanceVariableNames: 'browserClass svgExternalFile'
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

