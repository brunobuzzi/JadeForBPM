"Filed out from Dolphin Smalltalk 7"!

AllSessionsPresenter subclass: #BpmAllSessionsPresenter
	instanceVariableNames: 'gemsListPresenter'
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

onGemProcessSelected

	gemsListPresenter hasSelection ifFalse: [^sessionListPresenter resetSelection].

	sessionListPresenter selection: gemsListPresenter selection gsSession ifAbsent: []
!

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

stopAll
	| result command |

	command := WriteStream on: String new.
	command nextPutAll: 'System performOnServer: ';
			nextPutAll: '''cd $GS_HOME/shared/repos/BpmFlow/scripts; ';
			nextPutAll: 'sh stop-all.sh '''.

	result := gciSession executeString: command contents.

	MessageBox notify: result.

	self fillSessionList! !
!BpmAllSessionsPresenter categoriesFor: #createComponents!public! !
!BpmAllSessionsPresenter categoriesFor: #createSchematicWiring!public! !
!BpmAllSessionsPresenter categoriesFor: #fillSessionList!public!updating! !
!BpmAllSessionsPresenter categoriesFor: #onGemProcessSelected!public! !
!BpmAllSessionsPresenter categoriesFor: #startAll!public! !
!BpmAllSessionsPresenter categoriesFor: #stopAll!public! !

!BpmAllSessionsPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 nil nil 34 2 8 1409286144 131073 416 nil 721158 ##(Smalltalk.SystemColor) 31 nil 5 nil nil nil 416 1180166 ##(Smalltalk.ProportionalLayout) 170 176 8 #() true 170 192 560 nil 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1581 21 706 1153 621 416 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 22 3 0 0 10 0 0 0 86 5 0 0 64 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 816 nil nil nil 5 nil nil nil 816 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.CheckBox) 34 16 nil 816 34 2 8 1409363203 1 944 721990 2 ##(Smalltalk.ValueHolder) nil nil 1114118 ##(Smalltalk.NeverSearchPolicy) false nil nil 5 nil nil nil 944 nil 8 1919058316 852486 ##(Smalltalk.NullConverter) nil nil nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 3 255 706 201 45 944 658 #text: 34 1 8 'Auto-update' 944 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 127 0 0 0 101 0 0 0 149 0 0 0] 8 #() 706 193 193 nil 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 3 1378 #fixedViewLeft 201 1378 #fixedParentBottom -49 1378 #fixedViewTop 45 410 ##(Smalltalk.PushButton) 34 20 nil 816 34 2 8 1140924416 1 1456 nil nil nil 5 nil nil nil 1456 nil 8 1919058316 1180998 4 ##(Smalltalk.CommandDescription) #fillSessionList 8 'Update' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 1011 253 706 141 51 1456 658 #isEnabled: 8 #(false) 1456 658 #text: 34 1 8 'Update' 1456 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 249 1 0 0 126 0 0 0 63 2 0 0 151 0 0 0] 8 #() 1328 nil 29 1346 1378 #fixedViewRight -139 1378 #fixedParentRight -1 1424 -51 1440 51 410 ##(Smalltalk.ListView) 34 30 nil 816 34 2 8 1140920397 1025 1872 590662 2 ##(Smalltalk.ListModel) 138 144 560 nil 1310726 ##(Smalltalk.IdentitySearchPolicy) 786694 ##(Smalltalk.ColorDefault) 8 4278190080 nil 5 265030 4 ##(Smalltalk.Menu) nil true 34 4 984134 2 ##(Smalltalk.CommandMenuItem) 1 1538 #fillSessionList 8 '&Update' 1 1 nil nil nil 2114 1 1538 #sendSigAbort 8 'Send Sig&Abort' 1 1 nil nil nil 2114 1 1538 #sendSigUsr1 8 'Request &Stack' 1 1 nil nil nil 2114 1 1538 #stopSession 8 'S&top Session' 1 1 nil nil nil 8 '' nil 1 nil nil nil nil nil nil nil 1872 nil 8 1919214959 ##(Smalltalk.BasicListAbstract) nil 1049926 1 ##(Smalltalk.IconImageManager) nil nil nil nil nil nil 138 144 34 21 920646 5 ##(Smalltalk.ListViewColumn) 8 '#' 61 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 787814 3 ##(Smalltalk.BlockClosure) 0 459302 ##(Smalltalk.Context) 1 1 nil nil 1180966 ##(Smalltalk.CompiledExpression) 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each id]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #id 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Serial' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each serial]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #serial 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Description' 201 #left 459270 ##(Smalltalk.Message) #cacheDesc 8 #() 2866 #<= 2896 nil nil 1872 nil 1 nil nil 2418 8 'User' 201 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each name]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #name 17 257 nil nil 1872 nil 1 nil nil 2418 8 'View Age' 141 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 nil 2530 2 1 2464 8 'doIt' 8 '[:each | each viewAge]' 8 #[30 105 226 0 106] #viewAge 3136 7 257 nil nil 1872 nil 1 nil nil 2418 8 'Oldest' 121 #center ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each hasOldestCR]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #hasOldestCR 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Gem Host' 281 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each host]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #host 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Process' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each process]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #process 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Primitive' 131 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each primitive]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #primitive 17 257 nil nil 1872 nil 1 nil nil 2418 8 'State' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each state]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #state 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Trans' 111 #right ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each transaction]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #transaction 17 257 nil nil 1872 nil 1 nil nil 2418 8 'GCI IP' 241 #left ##(Smalltalk.BasicListAbstract) ##(Smalltalk.SortedCollection) 2466 0 2498 1 1 nil nil 2530 1 9 ##(Smalltalk.UndefinedObject) 8 'doIt' 34 2 8 '[:each | each ip]' 34 1 138 ##(Smalltalk.PoolDictionary) 560 8 #[252 1 0 1 1 5 0 17 229 32 158 106 105] #ip 17 257 nil nil 1872 nil 1 nil nil 2418 8 'Priority' 111 #right 2866 #displayString 8 #() 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each priority]' 8 #[30 105 226 0 106] #priority 4528 7 257 nil nil 1872 nil 1 nil nil 2418 8 'Host ID' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each hostId]' 8 #[30 105 226 0 106] #hostId 4672 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Unique host ID of the host where the session is running (an Integer)'']' 8 #[30 105 29 106] 8 'Unique host ID of the host where the session is running (an Integer)' 4752 7 257 nil nil 2418 8 'Quiet' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each quietTime]' 8 #[30 105 226 0 106] #quietTime 4912 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session''''s most recent request to the stone'']' 8 #[30 105 29 106] 8 'Seconds since the session''s most recent request to the stone' 4992 7 257 nil nil 2418 8 'Age' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each lifeTime]' 8 #[30 105 226 0 106] #lifeTime 5152 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Seconds since the session logged in'']' 8 #[30 105 29 106] 8 'Seconds since the session logged in' 5232 7 257 nil nil 2418 8 'Backlog' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each backlog]' 8 #[30 105 226 0 106] #backlog 5392 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of commits which have occurred since the session obtained its view'']' 8 #[30 105 29 106] 8 'Number of commits which have occurred since the session obtained its view' 5472 7 257 nil nil 2418 8 'Type' 201 #left 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 2464 8 'doIt' 8 '[:each | each description]' 8 #[30 105 226 0 106] #description 5632 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Nil or a String describing a system or GC gem'']' 8 #[30 105 29 106] 8 'Nil or a String describing a system or GC gem' 5712 7 257 nil nil 2418 8 'Objects' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each objects]' 8 #[30 105 226 0 106] #objects 5872 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (unused) object IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (unused) object IDs allocated to the session' 5952 7 257 nil nil 2418 8 'Pages' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each pages]' 8 #[30 105 226 0 106] #pages 6112 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''Number of temporary (non-persistent) page IDs allocated to the session'']' 8 #[30 105 29 106] 8 'Number of temporary (non-persistent) page IDs allocated to the session' 6192 7 257 nil nil 2418 8 'Voting' 121 #right 2866 #displayString 4496 2866 #<= 4496 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | each voteState]' 8 #[30 105 226 0 106] #voteState 6352 7 257 nil nil 1872 nil 1 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:each | ''0: session has not voted; 1: voting now; 2: voted'']' 8 #[30 105 29 106] 8 '0: session has not voted; 1: voting now; 2: voted' 6432 7 257 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 3 706 1153 251 1872 658 #contextMenu: 34 1 2080 1872 658 #text: 34 1 8 '#' 1872 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 1 0 0 0 65 2 0 0 126 0 0 0] 8 #() 1328 nil 27 1346 1392 3 1856 3 1378 #fixedParentTop 3 1424 -51 170 192 34 4 1872 8 'sessionList' 944 8 'autoUpdate' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 1153 305 816 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 64 2 0 0 152 0 0 0] 34 3 1872 1456 944 1328 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 416 34 2 8 1140850688 1 7056 nil 2032 nil 517 nil nil nil 1510470 1 ##(Smalltalk.DraggableViewInteractor) 7056 nil 1 #left nil nil nil 706 1 1 706 9 9 nil 7152 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 305 706 1153 11 7056 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 152 0 0 0 64 2 0 0 157 0 0 0] 8 #() 1328 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 416 34 2 8 1140850688 131073 7344 nil nil nil 5 nil nil nil 7344 514 170 176 560 false 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 315 706 1153 307 7344 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 157 0 0 0 64 2 0 0 54 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 nil 7344 34 2 8 1140850688 131073 7616 nil nil nil 5 nil nil nil 7616 882 170 176 34 4 410 ##(Smalltalk.GroupBox) 34 14 nil 7616 34 2 8 1140850695 65 7728 nil 590086 ##(Smalltalk.ColorNone) 8 4294967295 nil 5 nil nil nil 7728 nil 8 1919058316 594 138 144 34 2 658 #createAt:extent: 34 2 706 11 7 706 551 291 7728 658 #text: 34 1 8 'Running Web Servers' 7728 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 3 0 0 0 24 1 0 0 148 0 0 0] 8 #() 1328 nil 27 1346 1392 11 1856 -9 6816 7 1424 -9 410 ##(Smalltalk.ListView) 34 30 nil 7616 34 2 8 1409355853 1025 8080 1938 138 144 560 nil 2000 2032 nil 5 nil nil nil 8080 nil 8 1919214959 2866 #displayString 8 #() ##(Smalltalk.IconicListAbstract) 2368 nil nil nil nil nil nil 138 144 34 3 2418 8 'Name' 501 #left 2866 #name 8 #() 2466 0 nil 2530 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:a :b | a name <= b name]' 8 #[30 105 226 0 226 32 130 106] #name 8320 7 513 nil nil nil 8080 nil 1 nil nil 2418 8 'PID' 301 #left 2866 #pid 8304 2466 0 nil 2530 2 1 2464 8 'doIt' 8 '[:a :b | a pid <= b pid]' 8 #[30 105 226 0 226 32 130 106] #pid 8448 7 513 nil nil nil 8080 nil 1 nil nil 2418 8 'Port' 301 #left 2866 #port 8304 2466 0 nil 2530 2 1 2464 8 'doIt' 8 '[:a :b | a port <= b port]' 8 #[30 105 226 0 226 32 130 106] #port 8576 7 513 nil nil nil 8080 nil 1 nil nil #report 560 nil 131169 nil 34 4 nil nil 706 1 1 nil 594 138 144 34 2 658 #createAt:extent: 34 2 706 31 51 706 511 227 8080 658 #text: 34 1 8 'Name' 8080 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 15 0 0 0 25 0 0 0 14 1 0 0 138 0 0 0] 8 #() 1328 nil 27 1346 1392 31 1856 -29 6816 51 1424 -29 170 192 34 2 8080 8 'gemsList' nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 1 1 706 571 307 7616 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 29 1 0 0 153 0 0 0] 34 2 8080 7728 1328 nil 27 410 ##(Smalltalk.Splitter) 34 12 nil 7344 34 2 8 1140850688 1 9120 nil 2032 nil 517 nil nil nil 7122 9120 nil 1 #left nil nil nil 7152 7168 nil 7152 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 571 1 706 11 307 9120 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 29 1 0 0 0 0 0 0 34 1 0 0 153 0 0 0] 8 #() 1328 nil 27 410 ##(Smalltalk.ContainerView) 34 15 nil 7344 34 2 8 1140850688 131073 9360 nil nil nil 5 nil nil nil 9360 nil 170 192 560 nil 594 138 144 34 1 658 #createAt:extent: 34 2 706 581 1 706 573 307 9360 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 34 1 0 0 0 0 0 0 64 2 0 0 153 0 0 0] 34 2 410 ##(Smalltalk.PushButton) 34 20 nil 9360 34 2 8 1140924416 1 9600 nil nil nil 5 nil nil nil 9600 nil 8 1919058316 1538 #startAll 8 'Start All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 21 706 141 115 9600 658 #isEnabled: 8 #(false) 9600 658 #text: 34 1 8 'Start All Web Servers' 9600 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 10 0 0 0 71 0 0 0 67 0 0 0] 8 #() 1328 nil 29 410 ##(Smalltalk.PushButton) 34 20 nil 9360 34 2 8 1140924416 1 9952 nil nil nil 5 nil nil nil 9952 nil 8 1919058316 1538 #stopAll 8 'Stop All Web Servers' 1 1 nil nil false nil nil nil 594 138 144 34 3 658 #createAt:extent: 34 2 706 3 157 706 141 119 9952 658 #isEnabled: 8 #(false) 9952 658 #text: 34 1 8 'Stop All Web Servers' 9952 754 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 1 0 0 0 78 0 0 0 71 0 0 0 137 0 0 0] 8 #() 1328 nil 29 1328 nil 27 1328 nil 27 1328 nil 27 )! !
!BpmAllSessionsPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

