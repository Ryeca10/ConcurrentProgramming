-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
  get_ship_helper(Shipping_State#shipping_state.ships, Ship_ID).

get_ship_helper([], _Ship_ID) -> error;
get_ship_helper([Ship = #ship{id = Ship_ID} |_], Ship_ID) -> Ship;
get_ship_helper([_ | Other_Ships ], Ship_ID) -> get_ship_helper(Other_Ships, Ship_ID).

get_container(Shipping_State, Container_ID) ->
    get_container_helper(Shipping_State#shipping_state.containers, Container_ID).

get_container_helper([], Container_ID) -> error;
get_container_helper([ Container = #container{id = Container_ID} | _ ], Container_ID) -> Container;
get_container_helper( [_ | Other_Containers ], Container_ID) -> get_port_helper(Other_Containers, Container_ID).


get_port(Shipping_State, Port_ID) ->
      get_port_helper(Shipping_State#shipping_state.ports, Port_ID).

%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).

get_occupied_docks(Shipping_State, Port_ID) ->
     get_occupied_docks_helper(Shipping_State#shipping_state.ship_locations, Port_ID).

get_occupied_docks_helper ([], _Port_ID) -> [];
get_occupied_docks_helper ( [ {Port_ID, Dock ,_} | T ], Port_ID) -> [ Dock | get_occupied_docks_helper(T,Port_ID)];
get_occupied_docks_helper ( [ _ | T ], Port_ID) -> get_occupied_docks_helper(T,Port_ID).


get_ship_location(Shipping_State, Ship_ID) ->
    get_ship_location_helper(Shipping_State#shipping_state.ship_locations, Ship_ID).

get_ship_location_helper([], _Ship_ID) -> error;
get_ship_location_helper( [ {Port_ID, Dock , Ship_ID} | _ ], Ship_ID) -> {Port_ID, Dock};
get_ship_location_helper( [ _ | T ], Ship_ID) -> get_ship_location_helper(T, Ship_ID).

get_container_weight(Shipping_State, Container_IDs) ->
            case Container_IDs of 
                [] -> 0;
                [H|T] -> 
                          case catch(get_container_weight_helper(Shipping_State#shipping_state.containers, H)) of
                            error -> error;
                            _ -> get_container_weight_helper(Shipping_State#shipping_state.containers, H) +
                                 get_container_weight(Shipping_State, T)
                          end
            end.
      

get_container_weight_helper([ #container{id = Container_ID, weight = Weight} | _ ], Container_ID) -> Weight;
get_container_weight_helper([ _ | Other_Containers], Container_ID) -> get_container_weight_helper(Other_Containers, Container_ID);
get_container_weight_helper([], _Container_ID) -> error.


get_ship_weight(Shipping_State, Ship_ID) ->
   case catch(maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory)) of
        error -> error;
        {ok, List} ->  get_container_weight(Shipping_State,List)
    end.


load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {Port_ID,_} = get_ship_location(Shipping_State, Ship_ID),
    Bool = container_IDs_in_port_inventory(Shipping_State#shipping_state.port_inventory, Port_ID, Container_IDs),
    A = get_ship_container_cap(Shipping_State#shipping_state.ships, Ship_ID), % ship cap of ship_id
    B = foldr (fun(_,_) -> 1+1 end, 0 , Container_IDs), % number of containers in container_ids
    C = get_ship_inventory(Shipping_State#shipping_state.ship_inventory,Ship_ID), % containers already on ship of ship_id
    case (A - (B+C) > 0) and Bool of
          true -> newCo(Shipping_State,Ship_ID,Port_ID,Container_IDs);
          false -> error
    end.

container_IDs_in_port_inventory(PortInventory, Port_ID, Container_IDs) ->
    case catch(maps:find(Port_ID, PortInventory)) of
      error -> false;
      {ok,List} ->   matchLists(Container_IDs,List)
    end.

matchLists([],_List) -> true;
matchLists([H|T],List) ->
      case findHeadinList(H,List) of
        false-> false;
        true->matchLists(T,List)
      end.

findHeadinList(_Head,[]) -> false;
findHeadinList(H,[H|_]) -> true;
findHeadinList(Head,[_|T]) -> findHeadinList(Head,T).


get_ship_container_cap([],_Ship_ID) -> 0;
get_ship_container_cap([ #ship{id = Ship_ID, name = _ , container_cap = CC}|_] ,Ship_ID) -> CC;
get_ship_container_cap([_|T], Ship_ID ) -> get_ship_container_cap(T,Ship_ID).

newCo(Shipping_State,Ship_ID,Port_ID, Container_IDs) -> 
  Ships = Shipping_State#shipping_state.ships,
  Containers = Shipping_State#shipping_state.containers,
  Ports = Shipping_State#shipping_state.ports,
  Locations = Shipping_State#shipping_state.ship_locations,
  Ship_Inventory = update_ship_inventory(Shipping_State#shipping_state.ship_inventory,Ship_ID, Container_IDs),
  Port_Inventory = update_port_inventory(Shipping_State#shipping_state.port_inventory, Port_ID, Container_IDs),
 #shipping_state{ships = Ships, 
                           containers = Containers, 
                           ports = Ports, 
                           ship_locations = Locations, 
                           ship_inventory = Ship_Inventory, 
                           port_inventory = Port_Inventory}.


  newCoUnloadAll(Shipping_State,Ship_ID,Port_ID, Container_IDs) -> 
  Ships = Shipping_State#shipping_state.ships,
  Containers = Shipping_State#shipping_state.containers,
  Ports = Shipping_State#shipping_state.ports,
  Locations = Shipping_State#shipping_state.ship_locations,
  Port_Inventory = update_ship_inventory(Shipping_State#shipping_state.port_inventory, Port_ID, Container_IDs),
  Ship_Inventory = maps:update(Ship_ID, [] , Shipping_State#shipping_state.ship_inventory),
  %Ship_Inventory = update_port_inventory(Shipping_State#shipping_state.ship_inventory, Ship_ID, catch(maps:find (Ship_ID, Shipping_State#shipping_state.ship_inventory))),
  #shipping_state{ships = Ships, 
                  containers = Containers, 
                  ports = Ports, 
                  ship_locations = Locations, 
                  ship_inventory = Ship_Inventory, 
                  port_inventory = Port_Inventory}.


  newCoUnload(Shipping_State,Ship_ID,Port_ID, Container_IDs) -> 
  Ships = Shipping_State#shipping_state.ships,
  Containers = Shipping_State#shipping_state.containers,
  Ports = Shipping_State#shipping_state.ports,
  Locations = Shipping_State#shipping_state.ship_locations,
  Port_Inventory = update_port_inventory(Shipping_State#shipping_state.port_inventory, Port_ID, Container_IDs),
  Ship_Inventory = update_ship_inventory(Shipping_State#shipping_state.ship_inventory, Ship_ID, Container_IDs),
  #shipping_state{ships = Ships, 
                 containers = Containers, 
                 ports = Ports, 
                 ship_locations = Locations, 
                 ship_inventory = Map, 
                 port_inventory = Port_Inventory}.

  newCoSetSail(Shipping_State,Ship_ID,Port_ID, Dock) -> 
  Ships = Shipping_State#shipping_state.ships,
  Containers = Shipping_State#shipping_state.containers,
  Ports = Shipping_State#shipping_state.ports,
  Locations =  replace_port_and_dock(Shipping_State#shipping_state.ship_locations, Ship_ID, {Port_ID, Dock}),
  Port_Inventory = Shipping_State#shipping_state.port_inventory,
  Ship_Inventory = Shipping_State#shipping_state.ship_inventory,
  #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.




get_ship_inventory(Inventory, Ship_ID) ->
    case catch(maps:find(Ship_ID, Inventory)) of
        error -> error;
        {ok, List} ->  foldr (fun(_,_) -> 1+1 end, 0 , List)
    end.

foldr(_F,A,[]) -> A;
foldr(F,A,[H|T]) -> F(H, foldr(F,A,T)).

update_ship_inventory(ShipInventory,Ship_ID,Container_IDs) ->
  case catch( maps:find(Ship_ID, ShipInventory)) of
    error -> error;
    {ok, List} -> maps:update(Ship_ID, List++Container_IDs , ShipInventory)
  end.

update_port_inventory(PortInventory,Port_ID, Container_IDs) -> 
      case catch(maps:find(Port_ID, PortInventory)) of 
        error -> error;
        {ok,List} ->  Sorted1 = lists:sort(List),
                      Sorted2 = lists:sort(Container_IDs),
                      NewList = erase(Sorted1, Sorted2),
                      maps:update(Port_ID, NewList , PortInventory)
      end.

erase([],_Container_IDs) -> [];
erase([H|T],Container_IDs) ->
      case eraseHead(H,Container_IDs) of
          false -> [H|erase(T,Container_IDs)];
          true -> erase(T,Container_IDs)
      end.

eraseHead(Head,Container_IDs) ->
   case Container_IDs of
    [] -> false;
    [H|T] -> case H==Head of
                true -> true;
                false ->eraseHead(Head,T)
              end
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    % number of containers on the ship
    {Port_ID,_} = get_ship_location(Shipping_State, Ship_ID),
    #port{id=_ , name=_ , docks=_ , container_cap=CC} = get_port(Shipping_State, Port_ID),       % port capacity
    ContainersOnPort = get_number_of_containers_on_port_inventory(Port_ID, Shipping_State#shipping_state.port_inventory), % number of containers on the port
    ContainersOnShip = get_ship_inventory(Shipping_State#shipping_state.ship_inventory,Ship_ID),
    Containers = get_containers_on_ship(Ship_ID, Shipping_State#shipping_state.ship_inventory),
    case (CC - (ContainersOnPort+ContainersOnShip) > 0) of
          true -> newCoUnloadAll(Shipping_State,Ship_ID,Port_ID, Containers);
          false -> error
    end.


get_containers_on_ship(Ship_ID, ShipInventory) ->
  case catch(maps:find (Ship_ID, ShipInventory)) of
    error -> error;
    {ok,List} -> List
  end.

 get_number_of_containers_on_port_inventory(Port_ID, PortInventory) ->
    case catch(maps:find (Port_ID, PortInventory)) of 
        error -> error;
        {ok,List} -> foldr (fun(_,_) -> 1+1 end, 0 , List)
    end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    {Port_ID,_} = get_ship_location(Shipping_State, Ship_ID),
    #port{id=_ , name=_ , docks=_ , container_cap=CC} = get_port(Shipping_State, Port_ID),       % port capacity
    ContainersOnPort = get_number_of_containers_on_port_inventory(Port_ID, Shipping_State#shipping_state.port_inventory), % number of containers on the port
    ContainersOnShip = get_ship_inventory(Shipping_State#shipping_state.ship_inventory,Ship_ID),

    case (CC - (ContainersOnPort+ContainersOnShip) > 0) of
          true -> newCoUnload(Shipping_State,Ship_ID,Port_ID, Container_IDs);
          false -> error
    end.



set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    Occupied = isOccupiedPort(Shipping_State#shipping_state.ship_locations, Port_ID, Dock),
    case Occupied of
      true -> error;
      false -> newCoSetSail(Shipping_State,Ship_ID,Port_ID, Dock)
    end.
     
isOccupiedPort([], _Port_ID, _Dock) -> false;
isOccupiedPort([{Port_ID,Dock,_}|_], Port_ID, Dock) -> true;
isOccupiedPort([_|T], Port_ID, Dock) -> isOccupiedPort(T,Port_ID,Dock).


replace_port_and_dock([], _Ship_ID, {_Port_ID, _Dock}) -> [];
replace_port_and_dock([{_,_,Ship_ID}|T], Ship_ID, {Port_ID, Dock}) -> [{Port_ID,Dock,Ship_ID} | T];
replace_port_and_dock([H|T], Ship_ID, {Port_ID, Dock}) -> [H | replace_port_and_dock(T,Ship_ID,{Port_ID, Dock})].

%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],

    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],


    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},


    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },

    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
