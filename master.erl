-module(master).
-import(string,[len/1,concat/2,sub_string/3,equal/2]).
-import(crypto,[hash/2]).
-export([miner/4, spawner/4,start_mining/0,nodefunc/2,term/1]).

nodefunc(NodeName,N) ->
    %Calls client and invokes miners
    rpc:call(NodeName,worker,start_mining,[N]).

start_mining() ->
    {ok, N} = io:read("Enter the required Leading Zeroes: "),
    {_,_} = statistics(runtime),
    {_,_} = statistics(wall_clock),
    Pid = spawn(fun() -> term(-1) end),  

    %Checking for multiple node connections
    case is_alive() of
        %Multiple Systems Implementation
           true-> [nodefunc(NodeName,N)||NodeName<-nodes()],
                  Cores=erlang:system_info(logical_processors_available),
                  spawner(2*Cores,0,N,Pid);
        %Single System Implementation
           false-> io:fwrite("No nodes available!!"),
                   Cores=erlang:system_info(logical_processors_available),
                  spawner(2*Cores,0,N,Pid)


end.   
   

spawner(Cores,CoreCounter,N,Pid) ->
%Spawns Miners Recursively depending on available cores
if
        Cores >= CoreCounter ->
            
            spawn(master, miner,[N,0,0,Pid]),
        
            spawner(Cores,CoreCounter+1,N,Pid);  
            
        true ->
            ok
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

  HashedSubString=sub_string(HashString,1,N),

  % Create a string with N zeros

  ZeroString = string:copies("0",N),
  
  Status=string:equal(HashedSubString,ZeroString),

  %Checking for Leading Zeroes
    if
        StrCount =< 1000000 ->
        if
            Status == true ->
                io:format("~ts   ~ts ~n",[RandomString, HashString]),
                miner(N,StrCount+1,CoinCount+1,Pid);
            true ->
                miner(N,StrCount+1,CoinCount,Pid)
            
        end;
        true ->
            io:fwrite("The coins mined by ~p are ~p~n",[self(),CoinCount]),
            Pid ! {finished}
    end.
term(CoresDone) ->
    Cores = erlang:system_info(logical_processors_available), 
    %Utilization Ratio Calculation after termination of all miners
     if
        CoresDone == 2*Cores ->
                
                {_,T1} = statistics(runtime),
                {_,T2} = statistics(wall_clock),
                CPU_time = T1/ 1000,
                Run_time = T2 / 1000,
                T3 = CPU_time / Run_time,
                io:format("CPU time: ~p seconds\n", [CPU_time]),
                io:format("Real time: ~p seconds\n", [Run_time]),
                io:format("Ratio is ~p \n", [T3]);
            
        true ->
            receive
            {finished} ->
                term(CoresDone+1);
            Other ->
                ok
            end
    end.
    