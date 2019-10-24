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

startAll
	| result command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh start-all.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList
!

startPingLoop

	pingLoop := [ [true] whileTrue: [
			gemsListPresenter model do: [:bpmGemProc | | result |
				result := gciSession executeString: (self gemstoneBashCurlCommandFor: bpmGemProc).
				bpmGemProc setPingStatusFrom: result.
				(Delay forSeconds: JadeBpmCustomizationPreference default webServerCheckLoopDelay) wait.
			].
		].
	] forkAt: 4.!

stopAll
	| result command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh stop-all.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList!

stopPingLoop

	pingLoop ifNil: [^self].

	pingLoop terminate.

	pingLoop := nil! !
!BpmAllSessionsPresenter categoriesFor: #createComponents!public! !
!BpmAllSessionsPresenter categoriesFor: #createSchematicWiring!public! !
!BpmAllSessionsPresenter categoriesFor: #fillSessionList!public!updating! !
!BpmAllSessionsPresenter categoriesFor: #gemstoneBashCurlCommandFor:!public! !
!BpmAllSessionsPresenter categoriesFor: #onGemProcessSelected!public! !
!BpmAllSessionsPresenter categoriesFor: #pingPort!public! !
!BpmAllSessionsPresenter categoriesFor: #startAll!public! !
!BpmAllSessionsPresenter categoriesFor: #startPingLoop!public! !
!BpmAllSessionsPresenter categoriesFor: #stopAll!public! !
!BpmAllSessionsPresenter categoriesFor: #stopPingLoop!public! !

!BpmAllSessionsPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1333 21 706 1401 1001 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 154 2 0 0 10 0 0 0 86 5 0 0 254 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 816 nil nil nil 5 nil nil nil 816 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 nil 816 34 2 8 1140920397 1025 944 590662 2 ##(Smalltalk.ListModel) 138 144 560 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1180998 4 ##(Smalltalk.CommandDescription) #fillSessionList 8 '&Update' 1 1 nil nil nil 1186 1 1218 #sendSigAbort 8 'Send Sig&Abort' 1 1 nil nil nil 1186 1 1218 #sendSigUsr1 8 'Request &Stack' 1 1 nil nil nil 1186 1 1218 #stopSession 8 'S&top Session' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 944 nil 8 1919214959 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 21 920646 5 ##(Smalltalk.ListViewColumn) 8 '#' 61 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 459302 ##(Smalltalk.Context) 1 1 nil nil 1180966 ##(Smalltalk.CompiledExpression) 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each id]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #id 17 257 nil nil 944 nil 1 nil nil 1506 8 'Serial' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each serial]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #serial 17 257 nil nil 944 nil 1 nil nil 1506 8 'Description' 201 #left 459270 ##(Smalltalk.Message) #cacheDesc 8 #() 1954 #<= 1984 nil nil 944 nil 1 nil nil 1506 8 'User' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each name]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #name 17 257 nil nil 944 nil 1 nil nil 1506 8 'View Age' 141 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 nil 1618 2 1 1552 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] #viewAge 2224 7 257 nil nil 944 nil 1 nil nil 1506 8 'Oldest' 121 #center ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each hasOldestCR]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #hasOldestCR 17 257 nil nil 944 nil 1 nil nil 1506 8 'Gem Host' 281 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each host]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #host 17 257 nil nil 944 nil 1 nil nil 1506 8 'Process' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each process]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #process 17 257 nil nil 944 nil 1 nil nil 1506 8 'Primitive' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each primitive]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #primitive 17 257 nil nil 944 nil 1 nil nil 1506 8 'State' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each state]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #state 17 257 nil nil 944 nil 1 nil nil 1506 8 'Trans' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each transaction]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #transaction 17 257 nil nil 944 nil 1 nil nil 1506 8 'GCI IP' 241 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 1554 0 1586 1 1 nil nil 1618 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each ip]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #ip 17 257 nil nil 944 nil 1 nil nil 1506 8 'Priority' 111 #right 1954 #displayString 8 #() 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] #priority 3616 7 257 nil nil 944 nil 1 nil nil 1506 8 'Host ID' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] #hostId 3760 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 3840 7 257 nil nil 1506 8 'Quiet' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] #quietTime 4000 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 4080 7 257 nil nil 1506 8 'Age' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] #lifeTime 4240 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 4320 7 257 nil nil 1506 8 'Backlog' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] #backlog 4480 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 4560 7 257 nil nil 1506 8 'Type' 201 #left 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 1552 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] #description 4720 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 4800 7 257 nil nil 1506 8 'Objects' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] #objects 4960 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 5040 7 257 nil nil 1506 8 'Pages' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] #pages 5200 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 5280 7 257 nil nil 1506 8 'Voting' 121 #right 1954 #displayString 3584 1954 #<= 3584 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] #voteState 5440 7 257 nil nil 944 nil 1 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 5520 7 257 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 3 706 1401 441 944 658 #contextMenu: 34 1 1152 944 658 #text: 34 1 8 '#' 944 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 189 2 0 0 221 0 0 0] 8 #() 706 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 3 5938 #fixedParentRight 3 5938 #fixedParentTop 3 5938 #fixedParentBottom -51 410 ##(Smalltalk.CheckBox) 34 16 nil 816 34 2 8 1409363203 1 6016 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 6016 nil 8 1919058316 852486 ##(Smalltalk.NullConverter) nil nil nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 3 445 706 201 45 6016 658 #text: 34 1 8 'Auto-update' 6016 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 222 0 0 0 101 0 0 0 244 0 0 0] 8 #() 5888 nil 27 5906 5952 3 5938 #fixedViewLeft 201 6000 -49 5938 #fixedViewTop 45 410 ##(Smalltalk.PushButton) 34 20 nil 816 34 2 8 1140924416 1 6448 nil nil nil 5 nil nil nil 6448 nil 8 1919058316 1218 #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1259 443 706 141 51 6448 658 #isEnabled: 8 #(false) 6448 658 #text: 34 1 8 'Update' 6448 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 117 2 0 0 221 0 0 0 187 2 0 0 246 0 0 0] 8 #() 5888 nil 29 5906 5938 #fixedViewRight -139 5968 -1 6000 -51 6432 51 170 192 34 4 944 8 'sessionList' 6016 8 'autoUpdate' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 1401 495 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 188 2 0 0 247 0 0 0] 34 3 944 6448 6016 5888 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 7056 nil 1104 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 7056 nil 1 #left nil nil nil 706 1 1 706 9 9 nil 7152 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 495 706 1401 11 7056 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 247 0 0 0 188 2 0 0 252 0 0 0] 8 #() 5888 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 7344 nil nil nil 5 nil nil nil 7344 514 170 176 560 false 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 505 706 1401 497 7344 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 252 0 0 0 188 2 0 0 244 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 7344 34 2 8 1140850688 131073 7616 nil nil nil 5 nil nil nil 7616 882 170 176 34 4 410 ##(Smalltalk.ListView) 34 30 nil 7616 34 2 8 1409355853 1025 7728 1010 138 144 560 nil 1072 1104 nil 5 1138 nil true 34 1 1186 1 1218 #pingPort 8 'Ping Port' 1 1 nil nil nil 8 '' nil 134217729 nil nil nil nil nil nil nil 7728 nil 8 1919214959 1954 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 1456 nil nil nil nil nil nil 138 144 34 3 1506 8 'Name' 501 #left 1954 #name 8 #() 1554 0 nil 1618 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:a :b | a name <= b name]' 8 #[30 105 226 0 226 32 130 106] #name 8064 7 513 nil nil nil 7728 nil 1 nil 1554 0 nil 1618 8 1 ##(Smalltalk.BpmGemProcess) 8 'doIt' 8 '[:ctx | ctx item pingFailed ifTrue: [ctx forecolor: Color red] ifFalse: [ctx forecolor: Color black]]' 8 #[36 105 226 0 159 122 17 47 161 180 106 17 47 164 180 106] #item #pingFailed 983558 ##(Smalltalk.VariableBinding) #Color ##(Smalltalk.Color) #red #forecolor: #ifTrue:ifFalse: #black 8144 7 257 nil 1506 8 'PID' 301 #left 1954 #pid 8048 1554 0 nil 1618 2 1 1552 8 'doIt' 8 '[:a :b | a pid <= b pid]' 8 #[30 105 226 0 226 32 130 106] #pid 8304 7 513 nil nil nil 7728 nil 1 nil nil 1506 8 'Port' 301 #left 1954 #port 8048 1554 0 nil 1618 2 1 1552 8 'doIt' 8 '[:a :b | a port <= b port]' 8 #[30 105 226 0 226 32 130 106] #port 8432 7 513 nil nil nil 7728 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 31 51 706 635 417 7728 658 #contextMenu: 34 1 7824 7728 658 #text: 34 1 8 'Name' 7728 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 25 0 0 0 76 1 0 0 233 0 0 0] 8 #() 5888 nil 27 5906 5952 31 5968 -29 5984 51 6000 -29 410 ##(Smalltalk.GroupBox) 34 14 nil 7616 34 2 8 1140850695 65 8800 nil 590086 ##(Smalltalk.ColorNone) 8 4294967295 nil 5 nil nil nil 8800 nil 8 1919058316 594 138 144 34 2 658 #createAt:extent: 34 2 706 11 7 706 675 481 8800 658 #text: 34 1 8 'Running Web Servers' 8800 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 3 0 0 0 86 1 0 0 243 0 0 0] 8 #() 5888 nil 27 5906 5952 11 5968 -9 5984 7 6000 -9 170 192 34 2 7728 8 'gemsList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 695 497 7616 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 91 1 0 0 248 0 0 0] 34 2 7728 8800 5888 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 7344 34 2 8 1140850688 1 9360 nil 1104 nil 517 nil nil nil 7122 9360 nil 1 #left nil nil nil 7152 7168 nil 7152 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 695 1 706 11 497 9360 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 91 1 0 0 0 0 0 0 96 1 0 0 248 0 0 0] 8 #() 5888 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7344 34 2 8 1140850688 131073 9600 nil nil nil 5 nil nil nil 9600 nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 705 1 706 697 497 9600 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 96 1 0 0 0 0 0 0 188 2 0 0 248 0 0 0] 34 4 410 ##(Smalltalk.PushButton) 34 20 nil 9600 34 2 8 1140924416 1 9840 nil nil nil 5 nil nil nil 9840 nil 8 1919058316 1218 #startAll 8 'Start All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 21 706 141 115 9840 658 #isEnabled: 8 #(false) 9840 658 #text: 34 1 8 'Start All Web Servers' 9840 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 10 0 0 0 71 0 0 0 67 0 0 0] 8 #() 5888 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9600 34 2 8 1140924416 1 10192 nil nil nil 5 nil nil nil 10192 nil 8 1919058316 1218 #stopAll 8 'Stop All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 139 706 141 119 10192 658 #isEnabled: 8 #(false) 10192 658 #text: 34 1 8 'Stop All Web Servers' 10192 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 69 0 0 0 71 0 0 0 128 0 0 0] 8 #() 5888 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9600 34 2 8 1140924416 1 10544 nil nil nil 5 nil nil nil 10544 nil 8 1919058316 1218 #startPingLoop 8 'Start Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 281 706 145 81 10544 658 #isEnabled: 8 #(false) 10544 658 #text: 34 1 8 'Start Ping Loop' 10544 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 140 0 0 0 73 0 0 0 180 0 0 0] 8 #() 5888 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9600 34 2 8 1140924416 1 10896 nil nil nil 5 nil nil nil 10896 nil 8 1919058316 1218 #stopPingLoop 8 'Stop Ping Loop' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 369 706 145 81 10896 658 #isEnabled: 8 #(false) 10896 658 #text: 34 1 8 'Stop Ping Loop' 10896 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 184 0 0 0 73 0 0 0 224 0 0 0] 8 #() 5888 nil 29 5888 nil 27 5888 nil 27 5888 nil 27 )! !
!BpmAllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

