"Filed out from Dolphin Smalltalk 7"!

AllSessionsPresenter subclass: #BpmAllSessionsPresenter
	instanceVariableNames: 'webSeverGemsListPresenter pingLoop backgroundGemsListPresenter'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BpmAllSessionsPresenter guid: (GUID fromString: '{2f772bfc-1573-4dc2-89d2-08d362e9d6e8}')!
BpmAllSessionsPresenter comment: ''!
!BpmAllSessionsPresenter categoriesForClass!Unclassified! !
!BpmAllSessionsPresenter methodsFor!

createComponents
	super createComponents.

	webSeverGemsListPresenter := self add: ListPresenter new name: 'webServersGemsList'.
	backgroundGemsListPresenter := self add: ListPresenter new name: 'backgroundGemsList'.!

createSchematicWiring

	super createSchematicWiring.

	webSeverGemsListPresenter when: #selectionChanged send: #onGemProcessSelected to: self!

fillSessionList
	| portPidString portPidStringList |

	super fillSessionList.
	portPidString := gciSession executeString: 'BpmGemsInfo getGemsPidAndPortString'.
	portPidStringList := portPidString subStrings: ','.
	webSeverGemsListPresenter model: ListModel new.
	backgroundGemsListPresenter model: ListModel new.
	portPidStringList do: 	[:portPid | | portPidArray gemProc gsSession |
			portPidArray := portPid subStrings: '>>'.
			gemProc := BpmGemProcess new port: portPidArray first; pid: portPidArray second; yourself.
			gsSession := sessionListPresenter model detect: [:each | each process printString = gemProc pid] ifNone: [].
			gsSession ifNotNil: [gemProc name: gsSession cacheDesc; gsSession: gsSession.
					webSeverGemsListPresenter model add: gemProc]
	].
	(sessionListPresenter model detect: [:gsSession | gsSession cacheDesc = 'TimersLoop'] ifNone: []) ifNotNil: [:gsSession | | gemProc |
			gemProc := BpmGemProcess new name: gsSession cacheDesc; port: 'N/A'; pid: gsSession process; gsSession: gsSession; yourself.
			backgroundGemsListPresenter model add: gemProc].
	(sessionListPresenter model detect: [:gsSession | gsSession cacheDesc = 'ScriptsLoop'] ifNone: []) ifNotNil: [:gsSession | | gemProc |
			gemProc := BpmGemProcess new name: gsSession cacheDesc; port: 'N/A'; pid: gsSession process; gsSession: gsSession; yourself.
			backgroundGemsListPresenter model add: gemProc]!

gemstoneBashCurlCommandFor: aBpmGemProcess
	| command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''curl http://localhost:', aBpmGemProcess port,'/bpmflow'''.

	^command contents!

onGemProcessSelected

	webSeverGemsListPresenter hasSelection ifFalse: [^sessionListPresenter resetSelection].

	sessionListPresenter selection: webSeverGemsListPresenter selection gsSession ifAbsent: []!

pingPort
	| result |

	webSeverGemsListPresenter hasSelection ifFalse: [^self].
	result := gciSession executeString: (self gemstoneBashCurlCommandFor: webSeverGemsListPresenter selection).
	MessageBox notify: result.
	webSeverGemsListPresenter selection setPingStatusFrom: result!

restartOnPort
	| result command |

	webSeverGemsListPresenter hasSelection ifFalse: [^self].
	command := WriteStream on: String new.
	command
		nextPutAll: 'System performOnServer: ';
		nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
		nextPutAll: 'sh start-on.sh ' , webSeverGemsListPresenter selection port , ''''.
	result := gciSession executeString: command contents.
	MessageBox notify: result.
	self fillSessionList!

startAll
	| result command |

	(MessageBox confirm: 'Start All Web Sessions?') ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-all.sh ', self stoneName , ''''.

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
			nextPutAll: 'sh start-on.sh ', self stoneName, ' ', port, ''''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

startPingLoop

	pingLoop := [[true] whileTrue: [webSeverGemsListPresenter model do: [:bpmGemProc |  | result |
							result := gciSession executeString: (self gemstoneBashCurlCommandFor: bpmGemProc).
							bpmGemProc setPingStatusFrom: result.
							(Delay forSeconds: JadeBpmCustomizationPreference default webServerCheckLoopDelay) wait.
							]
						]
			] forkAt: 4!

startScriptsServer
	| command |

	(MessageBox confirm: 'Start Scripts Session?') ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-deferred-scripts-loop.sh ', self stoneName , ''''.

	gciSession executeString: command contents.

	self fillSessionList.
!

startTimersServer
	| command |

	(MessageBox confirm: 'Start Timers Session?') ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-timers-loop.sh ', self stoneName , ''''.

	gciSession executeString: command contents.

	self fillSessionList.!

stoneName

	^gciSession executeString: 'BpmGemsInfo stoneName'!

stopAll
	| result command |

	(MessageBox confirm: 'Stop All Web Sessions?') ifFalse: [^self].

	self stopPingLoop.

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh stop-all.sh ', self stoneName , ''''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

stopOnPort
	| result command |

	(MessageBox confirm: 'Stop Port Session?') ifFalse: [^self].
	webSeverGemsListPresenter hasSelection ifFalse: [^self].
	command := WriteStream on: String new.
	command
		nextPutAll: 'System performOnServer: ';
		nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
		nextPutAll: 'sh stop-on.sh ' , self stoneName , ' ' , webSeverGemsListPresenter selection port , ''''.
	result := gciSession executeString: command contents.
	MessageBox notify: result.
	self fillSessionList!

stopPingLoop

	pingLoop ifNil: [^self].

	pingLoop terminate.

	pingLoop := nil!

stopScriptsServer
	| bpmGemProcess |

	bpmGemProcess := backgroundGemsListPresenter model detect: [:each | each name = 'ScriptsLoop'] ifNone: [^self].
	(MessageBox confirm: 'Stop Scripts Session?') ifFalse: [^self].
	bpmGemProcess stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList!

stopTimersServer
	| bpmGemProcess |

	bpmGemProcess := backgroundGemsListPresenter model detect: [:each | each name = 'TimersLoop'] ifNone: [^self].
	(MessageBox confirm: 'Stop Timers Session?') ifFalse: [^self].
	bpmGemProcess stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList! !
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
!BpmAllSessionsPresenter categoriesFor: #stoneName!public! !
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1333 21 706 1401 1001 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 154 2 0 0 10 0 0 0 86 5 0 0 254 1 0 0] 34 1 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 816 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 816 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'GemStone Sessions' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 ##(Smalltalk.SmallInteger) 8 'doIt' 8 'CardLabel text: ''GemStone Sessions'' iconBlock: [Icon fromFile: ''icons\GS128.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 983558 ##(Smalltalk.VariableBinding) #CardLabel 1024 1056 1186 #Icon ##(Smalltalk.Icon) 8 'icons\GS128.ico' #fromFile: #text:iconBlock: 1088 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 816 34 2 8 1140850688 131073 1248 nil nil nil 5 nil nil nil 1248 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.PushButton) 34 20 nil 1248 34 2 8 1140924416 1 1376 nil nil nil 5 nil nil nil 1376 nil 8 1866629516 1180998 4 ##(Smalltalk.CommandDescription) #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1243 889 706 141 51 1376 658 #isEnabled: 8 #(false) 1376 658 #text: 34 1 8 'Update' 1376 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 109 2 0 0 188 1 0 0 179 2 0 0 213 1 0 0] 8 #() 706 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedViewRight -139 1794 #fixedParentRight -1 1794 #fixedParentBottom -51 1794 #fixedViewTop 51 410 ##(Smalltalk.CheckBox) 34 16 nil 1248 34 2 8 1409363203 1 1872 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 1872 nil 8 1866629516 852486 ##(Smalltalk.NullConverter) nil nil nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 3 891 706 201 45 1872 658 #text: 34 1 8 'Auto-update' 1872 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 189 1 0 0 101 0 0 0 211 1 0 0] 8 #() 1744 nil 27 1762 1794 #fixedParentLeft 3 1794 #fixedViewLeft 201 1840 -49 1856 45 410 ##(Smalltalk.ListView) 34 30 nil 1248 34 2 8 1140920397 1025 2304 590662 2 ##(Smalltalk.ListModel) 1573710 1 ##(Smalltalk.STBSortedCollectionProxy) ##(Smalltalk.SortedCollection) 560 1179910 ##(Smalltalk.IntrosortAlgorithm) 1074 2 nil 918822 ##(Smalltalk.CompiledMethod) 5 515 ##(Smalltalk.ListViewColumn) #rowSortBlock 47153027 8 #[171 221 50 6 223 4 33 112 17 106 6 89 5 90 173 221 18 233 16 252 2 0 0 4 28 0 233 50 18 143 233 32 143 150 106 233 16 252 2 0 0 4 10 0 233 50 17 143 233 33 143 150 106 60 105] #isSortable #ifTrue:ifFalse: #isSortOrderInverted #ifTrue: 1074 0 nil 2496 19 257 nil 91 513 nil 1074 0 459302 ##(Smalltalk.Context) 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each name]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #name 17 257 nil ##(Smalltalk.SortedCollection) nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 896 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1458 #fillSessionList 8 '&Update' 1 1 nil nil nil 2786 1 1458 #sendSigAbort 8 'Send Sig&Abort' 1 1 nil nil nil 2786 1 1458 #sendSigUsr1 8 'Request &Stack' 1 1 nil nil nil 2786 1 1458 #stopSession 8 'S&top Session' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 2304 nil 8 1866786159 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil 9 138 144 34 21 920646 5 ##(Smalltalk.ListViewColumn) 8 '#' 61 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each id]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #id 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Serial' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each serial]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #serial 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Description' 201 #left 459270 ##(Smalltalk.Message) #cacheDesc 8 #() 3490 #<= 3520 nil nil 2304 nil 1 nil nil 3090 8 'User' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2544 nil 2304 nil 1 nil nil 3090 8 'View Age' 141 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] #viewAge 3616 7 257 nil nil 2304 nil 1 nil nil 3090 8 'Oldest' 121 #center ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each hasOldestCR]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #hasOldestCR 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Gem Host' 281 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each host]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #host 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Process' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each process]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #process 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Primitive' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each primitive]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #primitive 17 257 nil nil 2304 nil 1 nil nil 3090 8 'State' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each state]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #state 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Trans' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each transaction]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #transaction 17 257 nil nil 2304 nil 1 nil nil 3090 8 'GCI IP' 241 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2562 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each ip]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #ip 17 257 nil nil 2304 nil 1 nil nil 3090 8 'Priority' 111 #right 3490 #displayString 8 #() 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] #priority 5008 7 257 nil nil 2304 nil 1 nil nil 3090 8 'Host ID' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] #hostId 5152 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 5232 7 257 nil nil 3090 8 'Quiet' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] #quietTime 5392 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 5472 7 257 nil nil 3090 8 'Age' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] #lifeTime 5632 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 5712 7 257 nil nil 3090 8 'Backlog' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] #backlog 5872 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 5952 7 257 nil nil 3090 8 'Type' 201 #left 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] #description 6112 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 6192 7 257 nil nil 3090 8 'Objects' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] #objects 6352 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 6432 7 257 nil nil 3090 8 'Pages' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] #pages 6592 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 6672 7 257 nil nil 3090 8 'Voting' 121 #right 3490 #displayString 4976 3490 #<= 4976 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] #voteState 6832 7 257 nil nil 2304 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 6912 7 257 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 3 706 1385 887 2304 658 #contextMenu: 34 1 2752 2304 658 #text: 34 1 8 '#' 2304 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 181 2 0 0 188 1 0 0] 8 #() 1744 nil 27 1762 2272 3 1824 3 1794 #fixedParentTop 3 1840 -51 170 192 34 4 2304 8 'sessionList' 1872 8 'autoUpdate' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 9 53 706 1385 941 1248 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 184 2 0 0 240 1 0 0] 34 3 2304 1376 1872 1744 nil 27 994 1026 8 'BPM Flow Sessions' 1074 0 nil 1106 6 1 ##(Smalltalk.SmallInteger) 8 'doIt' 8 'CardLabel text: ''BPM Flow Sessions'' iconBlock: [Presenter icon]' 8 #[45 30 34 112 47 161 106 194 105] 1200 7568 1186 #Presenter ##(Smalltalk.Presenter) #icon #text:iconBlock: 7584 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 816 34 2 8 1140850688 131073 7680 nil nil nil 5 nil nil nil 7680 788230 ##(Smalltalk.BorderLayout) 1 1 410 ##(Smalltalk.ContainerView) 34 15 nil 7680 34 2 8 1140850688 131073 7776 nil nil nil 5 nil nil nil 7776 7746 1 1 nil nil 410 ##(Smalltalk.PushButton) 34 20 nil 7776 34 2 8 1140924416 1 7856 nil nil nil 5 nil nil nil 7856 nil 8 1866629516 1458 #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1245 1 706 141 71 7856 658 #isEnabled: 8 #(false) 7856 658 #text: 34 1 8 'Update' 7856 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 110 2 0 0 0 0 0 0 180 2 0 0 35 0 0 0] 8 #() 1744 nil 29 nil nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 1385 71 7776 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 180 2 0 0 35 0 0 0] 34 1 7856 1744 nil 27 nil nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 7680 34 2 8 1140850688 131073 8384 nil nil nil 5 nil nil nil 8384 514 170 176 560 false 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 71 706 1385 871 8384 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 35 0 0 0 180 2 0 0 214 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 8384 34 2 8 1140850688 131073 8656 nil nil nil 5 nil nil nil 8656 7746 1 1 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 8656 34 2 8 1140850688 131073 8736 nil nil nil 5 nil nil nil 8736 nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 571 706 687 301 8736 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 29 1 0 0 87 1 0 0 179 1 0 0] 34 4 410 ##(Smalltalk.PushButton) 34 20 nil 8736 34 2 8 1140924416 1 8976 nil nil nil 5 nil nil nil 8976 nil 8 1866629516 1458 #startAll 8 'Start All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 27 21 706 141 115 8976 658 #isEnabled: 8 #(false) 8976 658 #text: 34 1 8 'Start All Web Servers' 8976 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 0 0 0 10 0 0 0 83 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 8736 34 2 8 1140924416 1 9328 nil nil nil 5 nil nil nil 9328 nil 8 1866629516 1458 #stopAll 8 'Stop All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 27 141 706 141 115 9328 658 #isEnabled: 8 #(false) 9328 658 #text: 34 1 8 'Stop All Web Servers' 9328 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 0 0 0 70 0 0 0 83 0 0 0 127 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 8736 34 2 8 1140924416 1 9680 nil nil nil 5 nil nil nil 9680 nil 8 1866629516 1458 #startPingLoop 8 'Start Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 201 21 706 141 115 9680 658 #isEnabled: 8 #(false) 9680 658 #text: 34 1 8 'Start Ping Loop' 9680 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 10 0 0 0 170 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 8736 34 2 8 1140924416 1 10032 nil nil nil 5 nil nil nil 10032 nil 8 1866629516 1458 #stopPingLoop 8 'Stop Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 201 141 706 141 115 10032 658 #isEnabled: 8 #(false) 10032 658 #text: 34 1 8 'Stop Ping Loop' 10032 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 70 0 0 0 170 0 0 0 127 0 0 0] 8 #() 1744 nil 29 1744 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 8656 34 2 8 1140850688 131073 10384 nil nil nil 5 nil nil nil 10384 1314 170 176 34 4 410 ##(Smalltalk.GroupBox) 34 14 nil 10384 34 2 8 1140850695 65 10496 nil 590086 ##(Smalltalk.ColorNone) 8 4294967295 nil 5 nil nil nil 10496 nil 8 1866629516 594 138 144 34 2 658 #createAt:extent: 34 2 706 11 11 706 667 541 10496 658 #text: 34 1 8 'Running Web Servers' 10496 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 82 1 0 0 19 1 0 0] 8 #() 1744 nil 27 1762 2272 11 1824 -9 7296 11 1840 -19 410 ##(Smalltalk.ListView) 34 30 nil 10384 34 2 8 1409355853 1025 10848 2370 138 144 560 nil 2720 896 nil 5 2738 nil true 34 5 2786 1 1458 #pingPort 8 'Ping Port' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2786 1 1458 #restartOnPort 8 'Restart Web Server' 1 1 nil nil nil 2786 1 1458 #startOnPort 8 'Start Web Server on' 1 1 nil nil nil 2786 1 1458 #stopOnPort 8 'Stop Web Server' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 10848 nil 8 1866786159 3490 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 3040 nil nil nil nil nil nil 138 144 34 3 3090 8 'Name' 501 #left 3490 #name 8 #() 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:a :b | a name <= b name]' 8 #[30 105 226 0 226 32 130 106] #name 11360 7 513 nil nil nil 10848 nil 1 nil 1074 0 nil 1106 8 1 ##(Smalltalk.BpmGemProcess) 8 'doIt' 8 '[:ctx | ctx item pingFailed ifTrue: [ctx forecolor: Color red] ifFalse: [ctx forecolor: Color black]]' 8 #[36 105 226 0 159 122 17 47 161 180 106 17 47 164 180 106] #item #pingFailed 1186 #Color ##(Smalltalk.Color) #red #forecolor: #ifTrue:ifFalse: #black 11440 7 257 nil 3090 8 'PID' 301 #left 3490 #pid 11344 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:a :b | a pid <= b pid]' 8 #[30 105 226 0 226 32 130 106] #pid 11584 7 513 nil nil nil 10848 nil 1 nil nil 3090 8 'Port' 301 #left 3490 #port 11344 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:a :b | a port <= b port]' 8 #[30 105 226 0 226 32 130 106] #port 11712 7 513 nil nil nil 10848 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 31 61 706 627 471 10848 658 #contextMenu: 34 1 10944 10848 658 #text: 34 1 8 'Name' 10848 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 30 0 0 0 72 1 0 0 9 1 0 0] 8 #() 1744 nil 27 1762 2272 31 1824 -29 7296 61 1840 -39 170 192 34 2 10848 8 'webServersGemsList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 687 571 10384 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 87 1 0 0 29 1 0 0] 34 2 10848 10496 1744 nil 27 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 687 871 8656 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 87 1 0 0 179 1 0 0] 34 2 10384 8736 1744 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 8384 34 2 8 1140850688 1 12464 nil 896 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 12464 nil 1 #left nil nil nil 706 1 1 706 9 9 nil 12560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 687 1 706 11 871 12464 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 87 1 0 0 0 0 0 0 92 1 0 0 179 1 0 0] 8 #() 1744 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 8384 34 2 8 1140850688 131073 12752 nil nil nil 5 nil nil nil 12752 7746 1 1 nil 410 ##(Smalltalk.ContainerView) 34 15 nil 12752 34 2 8 1140850688 131073 12832 nil nil nil 5 nil nil nil 12832 nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 571 706 689 301 12832 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 29 1 0 0 88 1 0 0 179 1 0 0] 34 4 410 ##(Smalltalk.PushButton) 34 20 nil 12832 34 2 8 1140924416 1 13072 nil nil nil 5 nil nil nil 13072 nil 8 1866629516 1458 #startTimersServer 8 'Start Timers Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 27 21 706 141 115 13072 658 #isEnabled: 8 #(false) 13072 658 #text: 34 1 8 'Start Timers Server' 13072 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 0 0 0 10 0 0 0 83 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 12832 34 2 8 1140924416 1 13424 nil nil nil 5 nil nil nil 13424 nil 8 1866629516 1458 #stopTimersServer 8 'Stop Timers Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 27 141 706 141 115 13424 658 #isEnabled: 8 #(false) 13424 658 #text: 34 1 8 'Stop Timers Server' 13424 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 13 0 0 0 70 0 0 0 83 0 0 0 127 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 12832 34 2 8 1140924416 1 13776 nil nil nil 5 nil nil nil 13776 nil 8 1866629516 1458 #startScriptsServer 8 'Start Scripts Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 201 21 706 141 115 13776 658 #isEnabled: 8 #(false) 13776 658 #text: 34 1 8 'Start Scripts Server' 13776 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 10 0 0 0 170 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 12832 34 2 8 1140924416 1 14128 nil nil nil 5 nil nil nil 14128 nil 8 1866629516 1458 #stopScriptsServer 8 'Stop Scripts Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 201 141 706 141 115 14128 658 #isEnabled: 8 #(false) 14128 658 #text: 34 1 8 'Stop Scripts Server' 14128 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 100 0 0 0 70 0 0 0 170 0 0 0 127 0 0 0] 8 #() 1744 nil 29 1744 nil 27 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 12752 34 2 8 1140850688 131073 14480 nil nil nil 5 nil nil nil 14480 1314 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 14480 34 2 8 1409355853 1025 14592 2370 138 144 560 nil 2720 896 nil 5 nil nil nil 14592 nil 8 1866786159 3490 #displayString 11248 ##(Smalltalk.IconicListAbstract) 3040 nil nil nil nil nil nil 138 144 34 3 3090 8 'Name' 501 #left 3490 #name 11344 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 11392 11408 11424 #name 14800 7 513 nil nil nil 14592 nil 1 nil 1074 0 nil 1106 8 1 ##(Smalltalk.BpmGemProcess) 11472 11488 11504 #item #pingFailed 1186 #Color ##(Smalltalk.Color) #red #forecolor: #ifTrue:ifFalse: #black 14832 7 257 nil 3090 11552 301 #left 3490 #pid 11344 1074 0 nil 1106 2 1 1072 11616 11632 11648 #pid 14912 7 513 nil nil nil 14592 nil 1 nil nil 3090 11680 301 #left 3490 #port 11344 1074 0 nil 1106 2 1 1072 11744 11760 11776 #port 14976 7 513 nil nil nil 14592 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 31 61 706 629 471 14592 658 #text: 34 1 8 'Name' 14592 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 30 0 0 0 73 1 0 0 9 1 0 0] 8 #() 1744 nil 27 1762 2272 31 1824 -29 7296 61 1840 -39 410 ##(Smalltalk.GroupBox) 34 14 nil 14480 34 2 8 1140850695 65 15264 nil 10576 nil 5 nil nil nil 15264 nil 8 1866629516 594 138 144 34 2 658 #createAt:extent: 34 2 706 11 11 706 669 541 15264 658 #text: 34 1 8 'Background Gems' 15264 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 83 1 0 0 19 1 0 0] 8 #() 1744 nil 27 1762 2272 11 1824 -9 7296 11 1840 -19 170 192 34 2 14592 8 'backgroundGemsList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 689 571 14480 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 88 1 0 0 29 1 0 0] 34 2 14592 15264 1744 nil 27 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 697 1 706 689 871 12752 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 1 0 0 0 0 0 0 180 2 0 0 179 1 0 0] 34 2 14480 12832 1744 nil 27 1744 nil 27 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 9 53 706 1385 941 7680 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 184 2 0 0 240 1 0 0] 34 2 7776 8384 1744 nil 27 7680 170 192 560 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 816 34 2 8 1140916736 1 16144 2370 138 144 34 2 1040 7552 nil 2720 nil nil 1 nil nil nil 16144 nil 8 1866456798 1074 0 nil 2482 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 16272 7 257 nil 1074 0 nil 2482 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 16320 7 257 nil 3040 nil nil nil nil nil #smallIcons nil nil nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1 1 706 1401 1001 16144 658 #basicSelectionsByIndex: 34 1 8 #(2) 16144 658 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 16144 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 244 1 0 0] 8 #() 1744 nil 27 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 1401 1001 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 244 1 0 0] 34 3 1248 7680 16144 1744 nil 27 1744 nil 27 )! !
!BpmAllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

