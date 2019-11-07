"Filed out from Dolphin Smalltalk 7"!

AllSessionsPresenter subclass: #BpmAllSessionsPresenter
	instanceVariableNames: 'webSeverGemsListPresenter pingLoop'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BpmAllSessionsPresenter guid: (GUID fromString: '{2f772bfc-1573-4dc2-89d2-08d362e9d6e8}')!
BpmAllSessionsPresenter comment: ''!
!BpmAllSessionsPresenter categoriesForClass!Unclassified! !
!BpmAllSessionsPresenter methodsFor!

createComponents
	super createComponents.

	webSeverGemsListPresenter := self add: ListPresenter new name: 'gemsList'!

createSchematicWiring

	super createSchematicWiring.

	webSeverGemsListPresenter when: #selectionChanged send: #onGemProcessSelected to: self!

fillSessionList
	| portPidString portPidStringList |

	super fillSessionList.
	portPidString := gciSession executeString: 'BpmGemsInfo getGemsPidAndPortString'.
	portPidStringList := portPidString subStrings: ','.
	webSeverGemsListPresenter model: ListModel new.
	portPidStringList do: 	[:portPid | | portPidArray gemProc gsSession |
			portPidArray := portPid subStrings: '>>'.
			gemProc := BpmGemProcess new port: portPidArray first; pid: portPidArray second; yourself.
			gsSession := sessionListPresenter model detect: [:each | each process printString = gemProc pid] ifNone: [].
			gsSession ifNotNil: [gemProc name: gsSession cacheDesc; gsSession: gsSession.
					webSeverGemsListPresenter model add: gemProc]
	].
	(sessionListPresenter model detect: [:gsSession | gsSession cacheDesc = 'TimersLoop'] ifNone: []) ifNotNil: [:gsSession | | gemProc |
			gemProc := BpmGemProcess new name: gsSession cacheDesc; port: 'N/A'; pid: gsSession process; gsSession: gsSession; yourself.
			webSeverGemsListPresenter model add: gemProc].
	(sessionListPresenter model detect: [:gsSession | gsSession cacheDesc = 'ScriptsLoop'] ifNone: []) ifNotNil: [:gsSession | | gemProc |
			gemProc := BpmGemProcess new name: gsSession cacheDesc; port: 'N/A'; pid: gsSession process; gsSession: gsSession; yourself.
			webSeverGemsListPresenter model add: gemProc]!

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
			nextPutAll: 'sh start-deferred-scripts-loop.sh '''.

	gciSession executeString: command contents.

	self fillSessionList.
!

startTimersServer
	| command |

	(MessageBox confirm: 'Start Timers Session?') ifFalse: [^self].

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-timers-loop.sh '''.

	gciSession executeString: command contents.

	self fillSessionList.!

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
	webSeverGemsListPresenter hasSelection ifFalse: [^self].
	command := WriteStream on: String new.
	command
		nextPutAll: 'System performOnServer: ';
		nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
		nextPutAll: 'sh stop-on.sh ' , webSeverGemsListPresenter selection port , ''''.
	result := gciSession executeString: command contents.
	MessageBox notify: result.
	self fillSessionList!

stopPingLoop

	pingLoop ifNil: [^self].

	pingLoop terminate.

	pingLoop := nil!

stopScriptsServer
	| bpmGemProcess |

	bpmGemProcess := webSeverGemsListPresenter model detect: [:each | each name = 'ScriptsLoop'] ifNone: [^self].
	(MessageBox confirm: 'Stop Scripts Session?') ifFalse: [^self].
	bpmGemProcess stopSession.
	(Delay forSeconds: 1) wait.
	self fillSessionList!

stopTimersServer
	| bpmGemProcess |

	bpmGemProcess := webSeverGemsListPresenter model detect: [:each | each name = 'TimersLoop'] ifNone: [^self].
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

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1333 21 706 1401 1001 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 154 2 0 0 10 0 0 0 86 5 0 0 254 1 0 0] 34 1 410 ##(Smalltalk.CardContainer) 34 16 nil 416 34 2 8 1409286144 131073 816 nil 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 nil nil nil 816 655878 ##(Smalltalk.CardLayout) 138 144 34 2 721414 ##(Smalltalk.Association) 590662 1 ##(Smalltalk.CardLabel) 8 'GemStone Sessions' 787814 3 ##(Smalltalk.BlockClosure) 0 nil 1180966 ##(Smalltalk.CompiledExpression) 7 1 ##(Smalltalk.SmallInteger) 8 'doIt' 8 'CardLabel text: ''GemStone Sessions'' iconBlock: [Icon fromFile: ''icons\GS128.ico'']' 8 #[45 30 35 113 47 32 180 106 195 105] 983558 ##(Smalltalk.VariableBinding) #CardLabel 1024 1056 1186 #Icon ##(Smalltalk.Icon) 8 'icons\GS128.ico' #fromFile: #text:iconBlock: 1088 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 816 34 2 8 1140850688 131073 1248 nil nil nil 5 nil nil nil 1248 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.PushButton) 34 20 nil 1248 34 2 8 1140924416 1 1376 nil nil nil 5 nil nil nil 1376 nil 8 1866629516 1180998 4 ##(Smalltalk.CommandDescription) #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1243 889 706 141 51 1376 658 #isEnabled: 8 #(false) 1376 658 #text: 34 1 8 'Update' 1376 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 109 2 0 0 188 1 0 0 179 2 0 0 213 1 0 0] 8 #() 706 193 193 nil 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedViewRight -139 1794 #fixedParentRight -1 1794 #fixedParentBottom -51 1794 #fixedViewTop 51 410 ##(Smalltalk.ListView) 34 30 nil 1248 34 2 8 1140920397 1025 1872 590662 2 ##(Smalltalk.ListModel) 1573710 1 ##(Smalltalk.STBSortedCollectionProxy) ##(Smalltalk.SortedCollection) 560 1179910 ##(Smalltalk.IntrosortAlgorithm) 1074 2 nil 918822 ##(Smalltalk.CompiledMethod) 5 515 ##(Smalltalk.ListViewColumn) #rowSortBlock 47153027 8 #[171 221 50 6 223 4 33 112 17 106 6 89 5 90 173 221 18 233 16 252 2 0 0 4 28 0 233 50 18 143 233 32 143 150 106 233 16 252 2 0 0 4 10 0 233 50 17 143 233 33 143 150 106 60 105] #isSortable #ifTrue:ifFalse: #isSortOrderInverted #ifTrue: 1074 0 nil 2064 19 257 nil 91 513 nil 1074 0 459302 ##(Smalltalk.Context) 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each name]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #name 17 257 nil ##(Smalltalk.SortedCollection) nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 896 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1458 #fillSessionList 8 '&Update' 1 1 nil nil nil 2354 1 1458 #sendSigAbort 8 'Send Sig&Abort' 1 1 nil nil nil 2354 1 1458 #sendSigUsr1 8 'Request &Stack' 1 1 nil nil nil 2354 1 1458 #stopSession 8 'S&top Session' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1872 nil 8 1866786159 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil 9 138 144 34 21 920646 5 ##(Smalltalk.ListViewColumn) 8 '#' 61 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each id]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #id 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Serial' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each serial]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #serial 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Description' 201 #left 459270 ##(Smalltalk.Message) #cacheDesc 8 #() 3058 #<= 3088 nil nil 1872 nil 1 nil nil 2658 8 'User' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2112 nil 1872 nil 1 nil nil 2658 8 'View Age' 141 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] #viewAge 3184 7 257 nil nil 1872 nil 1 nil nil 2658 8 'Oldest' 121 #center ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each hasOldestCR]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #hasOldestCR 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Gem Host' 281 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each host]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #host 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Process' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each process]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #process 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Primitive' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each primitive]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #primitive 17 257 nil nil 1872 nil 1 nil nil 2658 8 'State' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each state]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #state 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Trans' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each transaction]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #transaction 17 257 nil nil 1872 nil 1 nil nil 2658 8 'GCI IP' 241 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1074 0 2130 1 1 nil nil 1106 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each ip]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #ip 17 257 nil nil 1872 nil 1 nil nil 2658 8 'Priority' 111 #right 3058 #displayString 8 #() 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] #priority 4576 7 257 nil nil 1872 nil 1 nil nil 2658 8 'Host ID' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] #hostId 4720 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 4800 7 257 nil nil 2658 8 'Quiet' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] #quietTime 4960 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 5040 7 257 nil nil 2658 8 'Age' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] #lifeTime 5200 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 5280 7 257 nil nil 2658 8 'Backlog' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] #backlog 5440 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 5520 7 257 nil nil 2658 8 'Type' 201 #left 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] #description 5680 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 5760 7 257 nil nil 2658 8 'Objects' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] #objects 5920 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 6000 7 257 nil nil 2658 8 'Pages' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] #pages 6160 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 6240 7 257 nil nil 2658 8 'Voting' 121 #right 3058 #displayString 4544 3058 #<= 4544 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] #voteState 6400 7 257 nil nil 1872 nil 1 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 6480 7 257 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 3 706 1385 887 1872 658 #contextMenu: 34 1 2320 1872 658 #text: 34 1 8 '#' 1872 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 181 2 0 0 188 1 0 0] 8 #() 1744 nil 27 1762 1794 #fixedParentLeft 3 1824 3 1794 #fixedParentTop 3 1840 -51 410 ##(Smalltalk.CheckBox) 34 16 nil 1248 34 2 8 1409363203 1 6896 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 6896 nil 8 1866629516 852486 ##(Smalltalk.NullConverter) nil nil nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 3 891 706 201 45 6896 658 #text: 34 1 8 'Auto-update' 6896 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 189 1 0 0 101 0 0 0 211 1 0 0] 8 #() 1744 nil 27 1762 6864 3 1794 #fixedViewLeft 201 1840 -49 1856 45 170 192 34 4 6896 8 'autoUpdate' 1872 8 'sessionList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 9 53 706 1385 941 1248 754 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 184 2 0 0 240 1 0 0] 34 3 1872 1376 6896 1744 nil 27 994 1026 8 'BPM Flow Sessions' 1074 0 nil 1106 6 1 ##(Smalltalk.SmallInteger) 8 'doIt' 8 'CardLabel text: ''BPM Flow Sessions'' iconBlock: [Presenter icon]' 8 #[45 30 34 112 47 161 106 194 105] 1200 7568 1186 #Presenter ##(Smalltalk.Presenter) #icon #text:iconBlock: 7584 11 1 nil nil 410 ##(Smalltalk.ContainerView) 34 15 nil 816 34 2 8 1140850688 131073 7680 nil nil nil 5 nil nil nil 7680 514 170 176 560 false 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 9 53 706 1385 941 7680 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 26 0 0 0 184 2 0 0 240 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 7680 34 2 8 1140850688 131073 7952 nil nil nil 5 nil nil nil 7952 1314 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 7952 34 2 8 1409355853 1025 8064 1938 138 144 560 nil 2288 896 nil 5 2306 nil true 34 5 2354 1 1458 #pingPort 8 'Ping Port' 1 1 nil nil nil 983366 1 ##(Smalltalk.DividerMenuItem) 4097 2354 1 1458 #restartOnPort 8 'Restart Web Server' 1 1 nil nil nil 2354 1 1458 #startOnPort 8 'Start Web Server on' 1 1 nil nil nil 2354 1 1458 #stopOnPort 8 'Stop Web Server' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 8064 nil 8 1866786159 3058 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 2608 nil nil nil nil nil nil 138 144 34 3 2658 8 'Name' 501 #left 3058 #name 8 #() 1074 0 nil 1106 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:a :b | a name <= b name]' 8 #[30 105 226 0 226 32 130 106] #name 8576 7 513 nil nil nil 8064 nil 1 nil 1074 0 nil 1106 8 1 ##(Smalltalk.BpmGemProcess) 8 'doIt' 8 '[:ctx | ctx item pingFailed ifTrue: [ctx forecolor: Color red] ifFalse: [ctx forecolor: Color black]]' 8 #[36 105 226 0 159 122 17 47 161 180 106 17 47 164 180 106] #item #pingFailed 1186 #Color ##(Smalltalk.Color) #red #forecolor: #ifTrue:ifFalse: #black 8656 7 257 nil 2658 8 'PID' 301 #left 3058 #pid 8560 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:a :b | a pid <= b pid]' 8 #[30 105 226 0 226 32 130 106] #pid 8800 7 513 nil nil nil 8064 nil 1 nil nil 2658 8 'Port' 301 #left 3058 #port 8560 1074 0 nil 1106 2 1 1072 8 'doIt' 8 '[:a :b | a port <= b port]' 8 #[30 105 226 0 226 32 130 106] #port 8928 7 513 nil nil nil 8064 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 31 51 706 627 861 8064 658 #contextMenu: 34 1 8160 8064 658 #text: 34 1 8 'Name' 8064 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 25 0 0 0 72 1 0 0 199 1 0 0] 8 #() 1744 nil 27 1762 6864 31 1824 -29 6880 51 1840 -29 410 ##(Smalltalk.GroupBox) 34 14 nil 7952 34 2 8 1140850695 65 9296 nil 590086 ##(Smalltalk.ColorNone) 8 4294967295 nil 5 nil nil nil 9296 nil 8 1866629516 594 138 144 34 2 658 #createAt:extent: 34 2 706 11 7 706 667 925 9296 658 #text: 34 1 8 'Running Web Servers' 9296 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 3 0 0 0 82 1 0 0 209 1 0 0] 8 #() 1744 nil 27 1762 6864 11 1824 -9 6880 7 1840 -9 170 192 34 2 8064 8 'gemsList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 687 941 7952 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 87 1 0 0 214 1 0 0] 34 2 8064 9296 1744 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 7680 34 2 8 1140850688 1 9856 nil 896 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 9856 nil 1 #left nil nil nil 706 1 1 706 9 9 nil 9952 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 687 1 706 11 941 9856 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 87 1 0 0 0 0 0 0 92 1 0 0 214 1 0 0] 8 #() 1744 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7680 34 2 8 1140850688 131073 10144 nil nil nil 5 nil nil nil 10144 nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 697 1 706 689 941 10144 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 92 1 0 0 0 0 0 0 180 2 0 0 214 1 0 0] 34 9 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 10384 nil nil nil 5 nil nil nil 10384 nil 8 1866629516 1458 #startAll 8 'Start All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 21 706 141 115 10384 658 #isEnabled: 8 #(false) 10384 658 #text: 34 1 8 'Start All Web Servers' 10384 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 10 0 0 0 71 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 10736 nil nil nil 5 nil nil nil 10736 nil 8 1866629516 1458 #stopAll 8 'Stop All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 139 706 141 119 10736 658 #isEnabled: 8 #(false) 10736 658 #text: 34 1 8 'Stop All Web Servers' 10736 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 69 0 0 0 71 0 0 0 128 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 11088 nil nil nil 5 nil nil nil 11088 nil 8 1866629516 1458 #startPingLoop 8 'Start Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 281 706 145 81 11088 658 #isEnabled: 8 #(false) 11088 658 #text: 34 1 8 'Start Ping Loop' 11088 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 140 0 0 0 73 0 0 0 180 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 11440 nil nil nil 5 nil nil nil 11440 nil 8 1866629516 1458 #stopPingLoop 8 'Stop Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 369 706 145 81 11440 658 #isEnabled: 8 #(false) 11440 658 #text: 34 1 8 'Stop Ping Loop' 11440 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 184 0 0 0 73 0 0 0 224 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 11792 nil nil nil 5 nil nil nil 11792 nil 8 1866629516 1458 #startTimersServer 8 'Start Timers Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 161 21 706 141 115 11792 658 #isEnabled: 8 #(false) 11792 658 #text: 34 1 8 'Start Timers Server' 11792 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 10 0 0 0 150 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 12144 nil nil nil 5 nil nil nil 12144 nil 8 1866629516 1458 #stopTimersServer 8 'Stop Timers Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 161 139 706 141 115 12144 658 #isEnabled: 8 #(false) 12144 658 #text: 34 1 8 'Stop Timers Server' 12144 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 80 0 0 0 69 0 0 0 150 0 0 0 126 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 12496 nil nil nil 5 nil nil nil 12496 nil 8 1866629516 1458 #startScriptsServer 8 'Start Scripts Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 319 21 706 141 115 12496 658 #isEnabled: 8 #(false) 12496 658 #text: 34 1 8 'Start Scripts Server' 12496 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 0 0 0 10 0 0 0 229 0 0 0 67 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 12848 nil nil nil 5 nil nil nil 12848 nil 8 1866629516 1458 #stopScriptsServer 8 'Stop Scripts Server' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 319 139 706 141 115 12848 658 #isEnabled: 8 #(false) 12848 658 #text: 34 1 8 'Stop Scripts Server' 12848 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 159 0 0 0 69 0 0 0 229 0 0 0 126 0 0 0] 8 #() 1744 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 10144 34 2 8 1140924416 1 13200 nil nil nil 5 nil nil nil 13200 nil 8 1866629516 1458 #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 541 21 706 141 51 13200 658 #isEnabled: 8 #(false) 13200 658 #text: 34 1 8 'Update' 13200 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 14 1 0 0 10 0 0 0 84 1 0 0 35 0 0 0] 8 #() 1744 nil 29 1744 nil 27 1744 nil 27 7680 170 192 560 nil 410 ##(Smalltalk.TabViewXP) 34 28 nil 816 34 2 8 1140916736 1 13568 1938 138 144 34 2 1040 7552 nil 2288 nil nil 1 nil nil nil 13568 nil 8 1866456798 1074 0 nil 2050 2 3 ##(Smalltalk.ListControlView) #defaultGetTextBlock 575230339 8 #[30 105 226 0 106] #displayString 13696 7 257 nil 1074 0 nil 2050 2 3 ##(Smalltalk.IconicListAbstract) #defaultGetImageBlock 579598755 8 #[30 105 226 0 106] #iconImageIndex 13744 7 257 nil 2608 nil nil nil nil nil #smallIcons nil nil nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1 1 706 1401 1001 13568 658 #basicSelectionsByIndex: 34 1 8 #(2) 13568 658 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 13568 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 244 1 0 0] 8 #() 1744 nil 27 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 1401 1001 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 244 1 0 0] 34 3 1248 7680 13568 1744 nil 27 1744 nil 27 )! !
!BpmAllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

