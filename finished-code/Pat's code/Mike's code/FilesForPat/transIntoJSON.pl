/*
%%%%:- det(transJobStatsIntoJSON/1).
%%%%:- det(translateJobInfo/1).
%%%%:- det(translateJobStats/1).
%%%%:- det(translateJobStatsAux/1).
%%%%:- det(translateEpisodeStat/1).
%%%%:- det(translateStat/1).
%%%%:- det(translateEpisode/1).
%%%%:- det(translateID/1).
*/

transJobStatsIntoJSON(JobStatsIn) :-
    reverse(JobStatsIn, JobStats),
    JobStats = [JobInfo | Stats],
    open(jsonStats, write, JSONStream, [alias(jsonStats)]),
    write(jsonStats, '{'),
    translateJobInfo(JobInfo),
    format(jsonStats, ',~n',[]),
    translateJobStats(Stats),
    write(jsonStats, '}'),
    close(jsonStats).

translateJobInfo(JobInfo) :-
    JobInfo =
    aJobInfo(DomainName, ProblemName, ProblemSize, Mode,
             OptimalCost, HeuristicName, LBCalculate),
    format(jsonStats, '\"jobInfo\" : {', []),
    format(jsonStats, '\"domain\" : \"~p\",~n  ', [DomainName]),
    format(jsonStats, '\"problem\" : ~p, ~n', [ProblemName]), 
    format(jsonStats, '\"domainSize\" : ~p, ~n', [ProblemSize]),
    format(jsonStats, '\"mode\" : \"~p\",~n', [Mode]),
    format(jsonStats, '\"cStar\" : ~p,~n', [OptimalCost]), 
    format(jsonStats, '\"heuristic\" : \"~p\",~n', [HeuristicName]), 
    format(jsonStats, '\"lbCalculate\" : \"~p\"~n', [LBCalculate]), 
    format(jsonStats, '}',[]).


translateJobStats(Stats) :-
    format(jsonStats, '\"episodes\" : [',[]),
    translateJobStatsAux(Stats),
    format(jsonStats, ']~n',[]).

translateJobStatsAux([]).

translateJobStatsAux([Episode | Rest]) :-
%%%%    print("1"),nl,
    translateEpisodeStat(Episode),
    (Rest = [] ->
         format(jsonStats, ']~n}',[])
    ;
         format(jsonStats, ']~n},',[]),
         translateJobStatsAux(Rest)
    ).
    
%% %% translateJobStatsAux([Episode, Episode1 | Rest]) :-
%% %%     print("2"),nl,
%% %%     translateEpisodeStat(Episode),
%% %%     format(jsonStats, ']*,*~n},',[]),
%% %%     translateJobStatsAux([Episode1 |Rest]).



%% %% translateJobStatsAux([Episode | Rest]) :-
%% %%     translateEpisodeStat(Episode),
%% %%     format(jsonStats, ']*,*~n},',[]),
%% %%     translateJobStatsAux(Rest).

%% %% translateJobStatsAuxAux(Episode) :-
%% %%     translateEpisodeStat(Episode).


%% %% [ expansionSeq([(1,"f25")]),
%% %%     solution([[("f25",1),("f26","nexp")]]),
%% %%     stat(id(-1,generate,backward,0,1,1),1),
%% %%     stat(id(-1,generate,forward,0,1,1),1),
%% %%     stat(id(-1,open,backward,0,1,1),1)]
  
translateEpisodeStat([ExpansionSeq, Solution| Episodes]) :-
    ExpansionSeq = expansionSeq(ExpSeq),
    format(jsonStats, '{\"expansionSeq\" : ',[]),
    parenthesisListToBrackets(ExpSeq, ExpSeqBrack),
    format(jsonStats, '~p,~n',[ExpSeqBrack]),
    Solution = solution([SolSeq]),
    format(jsonStats, '\"solution\" : ',[]),
    parenthesisListToBrackets(SolSeq, SolSeqBrack),
    format(jsonStats, '~p,~n', [SolSeqBrack]),
           format(jsonStats,'\"stats\" : ~n[',[]),
    Episodes = [Episode | _],
    Episode = stat(ID, Count),
    translateIDAux(ID),
    format(jsonStats, '\"count\" : ~p}~n',[Count]),
    translateStat(Episodes).

 
parenthesisListToBrackets(ListOfParens, ListOfBrackets) :-
    parenthesisListToBracketsAux(ListOfParens, [], ListOfBracks),
    reverse(ListOfBracks, ListOfBrackets).

parenthesisListToBracketsAux([], ListOfBrackets, ListOfBrackets).
parenthesisListToBracketsAux([(X,Y) | Rest], ParensIn, ParensOut) :-
    parenthesisListToBracketsAux(Rest, [[X,Y] | ParensIn], ParensOut).


%% [stat(id(-1,generate,backward,0,1,1),1),
    %% stat(id(-1,generate,forward,0,1,1),1),
    %% stat(id(-1,open,backward,0,1,1),1)]
translateStat([]).
translateStat([Episode | Rest]) :-
    translateEpisode(Episode),
    translateStat(Rest).

%% %% stat(id(-1,generate,backward,0,1,1),1)
translateEpisode(stat(ID, Count)) :-
    translateID(ID),
    format(jsonStats, '\"count\" : ~p}~n',[Count]).

translateID(ID) :-
    format(jsonStats, ',',[]),
    translateIDAux(ID).

translateIDAux(id(CActionAt, Action, Direction, G, H, LB)) :-
    format(jsonStats, '{\"cActionAt\" : ~p,~n', [CActionAt]),
    format(jsonStats, '\"action\" : \"~p\",~n', [Action]),
    format(jsonStats, '\"direction\" : \"~p\",~n', [Direction]),
    format(jsonStats, '\"g\" : ~p,~n', [G]),
    format(jsonStats, '\"h\" : ~p,~n', [H]),
    format(jsonStats, '\"lb\" : ~p,~n', [LB]).
