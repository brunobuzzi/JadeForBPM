"Filed out from Dolphin Smalltalk 7"!

Object subclass: #BpmGemProcess
	instanceVariableNames: 'name pid port gsSession pingFailed'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

icon

	^Icon fromFile: 'icons\GS128.ico'!

new

	^super new initialize! !
!BpmGemProcess class categoriesFor: #icon!public! !
!BpmGemProcess class categoriesFor: #new!public! !

