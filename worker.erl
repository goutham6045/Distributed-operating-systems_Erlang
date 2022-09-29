-module(worker).
-import(string,[len/1]).
-import(crypto,[hash/2]).
-import(string,[equal/2]).
-import(string,[concat/2,sub_string/3]).
-export([spawner/4,miner/4,start_mining/1,term/1]).

start_mining(N) ->
    io:format("Node"),
    Cores=erlang:system_info(logical_processors_available),
    Pid = spawn(fun() -> term(-1) end),
    spawner(2*Cores,0,N,Pid).

spawner(Cores,CoreCounter,N,Pid) ->
        if
            Cores >= CoreCounter ->
                
                spawn(worker, miner,[N,0,0,Pid]),
                
                spawner(Cores,CoreCounter+1,N,Pid);  
                
            true ->
                io:fwrite("Processes Invoked!!!")
        end.


miner(N,StrCount,CoinCount,Pid)->
    UfID="adloorih",

    %Generate a random string
    String=base64:encode_to_string(crypto:strong_rand_bytes(24)),

    %Append Random String with UFID
    RandomString=string:concat(UfID,String),
    %Hash the Random string using SHA256
    HashString=io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,RandomString))]),
    %Create a substring from first character to N character
    HashedSubString= sub_string(HashString,1,N),
    % Create a string with N zeros
    ZeroString = string:copies("0",N),

    Status=string:equal(HashedSubString,ZeroString),

        if
            StrCount =< 1000000 ->
            if
                Status == true ->
                %io:format("Bitcoin is found and sending message to server\n"),
                io:format("~ts  ~ts~n",[RandomString, HashString]),
                %io:format("Bitcoin Code Endsssssssssssssssss\n"),
                miner(N,StrCount+1,CoinCount+1,Pid);
                true ->
                    miner(N,StrCount+1,CoinCount,Pid)
            end;
            true ->
                io:format("All the strings are Done!~n"),
                io:fwrite("The coins mined by ~p are ~p~n",[self(),CoinCount]),
                Pid ! {finished}
        end.

term(CoresDone) ->
    Cores = erlang:system_info(logical_processors_available), 

    if
        CoresDone == 2*Cores ->
                
                {_,T1} = statistics(runtime),
                {_,T2} = statistics(wall_clock),
                CPU_time = T1/ 1000,
                Run_time = T2 / 1000,
                T3 = CPU_time / Run_time,
                io:format("CPU time: ~p seconds\n", [CPU_time]),
                io:format("Real time: ~p seconds\n", [Run_time]),
                io:format("Ratio of the Node ~p is ~p \n", [node(),T3]);
                      
            
        true ->
            receive
            {finished} ->
                io:fwrite("Core Computed! Core Count of CLIENTS ~p~n",[CoresDone]),
                term(CoresDone+1);
            Other ->
                ok
            end
    end.
