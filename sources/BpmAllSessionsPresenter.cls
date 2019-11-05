"Filed out from Dolphin Smalltalk 7"!

AllSessionsPresenter subclass: #BpmAllSessionsPresenter
	instanceVariableNames: 'gemsListPresenter pingLoop'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

	(MessageBox confirm: 'Start All Web Sessions?') ifFalse: [^self].

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

	(MessageBox confirm: 'Start Scripts Session?') ifFalse: [^self].

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

	(MessageBox confirm: 'Start Timers Session?') ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-timers-loop.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

stopAll
	| result command |

	(MessageBox confirm: 'Stop All Web Sessions?') ifFalse: [^self].

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

	(MessageBox confirm: 'Stop Port Session?') ifFalse: [^self].

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

	(MessageBox confirm: 'Stop Scripts Session?') ifFalse: [^self].

	bpmGemProcess stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList.!

stopTimersServer
	| bpmGemProcess |

	bpmGemProcess := gemsListPresenter model detect: [:each | each name = 'TimersLoop'] ifNone: [^self].

	(MessageBox confirm: 'Stop Timers Session?') ifFalse: [^self].

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

