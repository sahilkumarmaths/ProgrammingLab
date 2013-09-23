(*
 * Assignment 		: 2
 * Author     		: Sahil Kumar
 * Roll_No    		: 10010175 
 * Problem    		: Dijkstra Algorithm
 * Date Submitted	: 28/8/2013 
 *)

(*********************** Utility functions for List (parent_table) ***********************)
(*
	Function:
		Returns the distance for a given vertex
	Parameters: 
		Parent_table: This contains data
		Vertex: The parent of whose needs to find
    Returns:
		~3 = element not there
		-2 = nil
		int = distance
*)
fun parent_distance_vertex(nil,vertex) = ~2
	| parent_distance_vertex([(a,b,c)],vertex)=
	if(a=vertex) then
		b
	else
	    ~3
	| parent_distance_vertex((a,b,c) :: L,vertex)=
	  if(a=vertex) then
		b
	  else
	    parent_distance_vertex(L,vertex);

(*
	Function:
		Sets the distance for a given vertex
	Parameters:
		Parent_table: This contains data
		Vertex: Whose data needs to be changed
	Results:
		Parent_table: Table that stores the final data
		["",~1,""] = if table size is nil
		if element not there then does nothing
		if more than one vertex are there then updates just 1
 *)
fun parent_set_distance_vertex(nil,vertex,distance) = [("",~1,"")]
   | parent_set_distance_vertex([(a,b,c)],vertex,distance)=
	 if(a=vertex) then
		[(a,distance,c)]
	 else
	    [(a,b,c)]
	| parent_set_distance_vertex((a,b,c) :: L,vertex,distance)=
	  if(a=vertex) then
		(a,distance,c)::L
	  else
	    (a,b,c)::parent_set_distance_vertex(L,vertex,distance);
		
(*
	Function:
		Set parent for a particular vertex
	Parameters:
		Parent_table: This contains data
		Vertex: Whose data needs to be changed
		NParent: The new Parent
	Results:
		["",~1,""] = if table size is nil
		if element not there then does nothing
		if more than one vertex are there then updates just 1
*)
fun parent_set_parent_vertex(nil,vertex,nparent) = [("",~1,"")]
   | parent_set_parent_vertex([(a,b,c)],vertex,nparent)=
	 if(a=vertex) then
		[(a,b,nparent)]
	 else
	    [(a,b,c)]
	| parent_set_parent_vertex((a,b,c) :: L,vertex,nparent)=
	  if(a=vertex) then
		(a,b,nparent)::L
	  else
	    (a,b,c)::parent_set_parent_vertex(L,vertex,nparent);

(* 
	Function:
		Checks if the vertex exists in the Parent_table
	Parameters:
		Parent_table: This contains data
		Vertex: To check
	Results:
		0 means list is empty
		1 means exists
		-1 means does not exists
*)
 fun parent_vertex_exists(nil,vertex)= 0
 | parent_vertex_exists([(a,b,c)],vertex) =
	if (a=vertex) then
		1
    else
	    ~1
 | parent_vertex_exists((a,b,c)::L,vertex)=
 if(a=vertex) then
   1
 else
   parent_vertex_exists(L,vertex);
   
(*
    Function:
		Returns the parent_table in the concatenated string format
	Parameters:
		Parent_table: This contains data
	Result:
		Returns the string that can be printed.
 *)
fun print_parent_table(nil)="\n"
| print_parent_table((a,b,c)::L)= 
	a^"      "^c^"    "^ (Int.toString(b))^"\n"^print_parent_table(L); 

(*
	Function:
		Initialze the Parent_table
    Parameters:
		Parent_table: This contains data
		list_vertices: The list of all the vertices that needs to be added.
	Result:
		Parent_table: This contains data
 *)
 fun parent_table_init(parent_table,nil)=parent_table
   | parent_table_init(parent_table,a::l)=
   parent_table_init((a,~1,"-"):: parent_table ,l);

   
(* 
	Function:
		Returns the parent of the vertex
	Parameters:
		Parent_table: This contains data
		Vertex: The parent of whose needs to find
	Result:
		"": Vertex not found
		"t": The parent of the vertex
*)
 fun parent_vertex(nil,vertex)= ""
 | parent_vertex([(a,b,c)],vertex) =
	if (a=vertex) then
		c
    else
	    ""
 | parent_vertex((a,b,c)::L,vertex)=
 if(a=vertex) then
   c
 else
   parent_vertex(L,vertex);
   
(*
	Function:
		Prints the path
	Parameters:
		Parent_table: This contains data
		Source: the source vertex
		Destination: the destination vertex
	Results:
		Returns the path to the destination
	
*)
fun print_path(parent_table,source:string,"",l:string)= ""
  | print_path(parent_table,source:string,"-",l:string) = l 
  |print_path(parent_table,source:string,destination:string,l:string)=
  let
	val parent_destination = parent_vertex(parent_table,destination)
  in

	if( parent_destination= "") then
		l
	else	   
		print_path(parent_table,source,parent_destination,l)^" -> "^destination 
  end;
(*********************** Utility functions for Priority Queue ***********************)

 
(* 
	Function:
		This function inserts the vertex and distance in the list.
	Paramters:
		Priority_queue: Contains Data
		Vertex: The vertex to be inserted
		Distance: The distance of the vertex to be inserted
	Result:
		The new modified priority qyeue
*)
fun insert_vertex_distance(nil,vertex,distance) = [(vertex,distance)]
	| insert_vertex_distance(L,vertex,distance) = 
	let 
		val a = (vertex,distance);
	in
		a::L
	end;

(* 	
	Function:
		This function returns (vertex*distance) for the vertex that has minimum distance
	Parameters:
		Priority_queue: Contains Data
	Results:
		The minimum Vertex
*)
fun minimum_vertex_distance(nil) = ("",~1)
  | minimum_vertex_distance([a]) = (a)
  | minimum_vertex_distance(a::b) = 
  let 
	val vertex_a = #1a
    val distance_a = #2a
	val pair_b = minimum_vertex_distance(b)
	val vertex_b = #1pair_b
	val distance_b = #2pair_b
  in 
	if(distance_a<distance_b)
	then
		a
	else 
		pair_b
  end;

(* 
	Function:
		Deletes the (vertex*distance) for given vertex
	Parameters:
		Priority_queue: Contains Data
	Results:
		If passed nil returns [("",~1)]
		else list empty or the element removed
*)
fun delete_vertex(nil,vertex)= [("",~1)]
  | delete_vertex(a::L,vertex)= 
  let 
	val a_vertex = #1a
  in
	if(a_vertex = vertex ) then
		L
	else
		a::delete_vertex(L,vertex)
  end;
  
(* 
	Function:
		Updates the value for a given vertex to distance
	Parameters:
		Priority_queue: Contains Data
		Vertex: The vertex whose distance needs to be updated
		Distance: The new distance
	Results:
		The new updated queue
*)
fun set_vertex_distance(nil,vertex,distance) = [("",~1)]
  | set_vertex_distance(a::L,vertex,distance) =
  let
	val a_vertex = #1a
  in 
	if(a_vertex = vertex) then
	  insert_vertex_distance(L,vertex,distance)
	else
		a::set_vertex_distance(L,vertex,distance)
  end;

 (* 
	Function:
		Checks if the vertex exists in the List 
	Parameters:
		Priority_queue: Contains Data
		Vertex: The vertex that needs to be searched
	Results:
		0 means list is empty
		1 means exists
		-1 means does not exists
  *)
 fun vertex_exists(nil,vertex)= 0
 | vertex_exists([(a,b)],vertex) =
	if (a=vertex) then
		1
    else
	    ~1
 | vertex_exists((a,b)::L,vertex)=
 if(a=vertex) then
   1
 else
   vertex_exists(L,vertex);
   
(*********************** Some common Utility functions ***********************)
(* 
	Function:
		Checks if the vertex exists in the List
	Paramter:
		Priority_queue: Contains data
		Vertex: The vertex that needs to be searched
	Results:
		0 means list is empty
		1 means exists
		~1 means does not exists
  *)
 fun list_vertex_exists(nil,vertex:string)= 0
 | list_vertex_exists([a]:(string)list,vertex) =
	if (a=vertex) then
		1
    else
	    ~1
 | list_vertex_exists((a)::L,vertex)=
 if(a=vertex) then
   1
 else
   list_vertex_exists(L,vertex);

   
(*
   Function:
		Returns the list of all the Vertices in the Graph.
   Parameters: 
		Graph: the graph.
		List: the list of vertices
   Result:
		Returns the list of vertices.
   Assumption: 
		There are no edges of the form A-> A
 *)
fun graph_vertices(nil,nil) =nil
  | graph_vertices(nil,l)=l
  | graph_vertices((a,b,c)::L,l)=
  let 
	val exist_a = list_vertex_exists(l,a)
	val exist_b = list_vertex_exists(l,b)
  in
	if(a=b) then
		if(exist_a <> 1 ) then
			graph_vertices(L,a::l)
		else
			graph_vertices(L,l)
	else
		if(exist_a <> 1 andalso exist_b <> 1) then
			graph_vertices(L,a::b::l)
		else if(exist_a <>1) then
			graph_vertices(L,a::l)
		else if(exist_b <>1) then
			graph_vertices(L,b::l)
		else
			graph_vertices(L,l)
  end;
 
(*
	Function:
		Adjacent Neighbours of a Vertex in a graph
	Parameters:
		Graph: Contains Data
		Vertex: Whose vertices we need
	Results:
		List: list of neighbours
 *)
fun list_neighbours(nil,nil,vertex)=nil
| list_neighbours(nil ,t,vertex) = t
| list_neighbours((a,b,c)::L:(string*string*int)list ,t:(string)list,vertex:string) =
if(vertex = a andalso list_vertex_exists(t,b)<>1 ) then
	list_neighbours(L:(string*string*int)list,b::t,vertex)
else
	list_neighbours(L,t,vertex);
	
(*
	Function:
		Returns the weight of the edge u->v in the graph
	Parameters: 
		Graph: G
		Vertex: u
		Vertex: v
	Result:
		~2 = Graph is null or the edge does not exists in the graph
		int = weight of the edge
*)
fun graph_weight(nil,u:string,v:string)= ~2
  |  graph_weight((a,b,c)::L:(string*string*int)list,u,v)=
	if(a=u andalso b=v) then
		c
	else
		graph_weight(L,u,v);
  
(*
	Function:
		Final for loop in algorithm
	Parameters:
		Graph:Contains Data
		priority_queue: Contains Data
		parent_table: Contains Data
		adjacent_neighbours: The adjacent neighbours
		minimum_vertex: The minimum vertex
	Result:
		It is the recursive for loop that executes for the neighbours of the minimum vertex
*)
fun for_all_neighbours(Graph,priority_queue,parent_table,nil,minimum_vertex:(string*int))= (priority_queue, parent_table)
  | for_all_neighbours(Graph,priority_queue,parent_table,a::adjacent_neighbours,minimum_vertex) = 
  let 
	val vertex = #1minimum_vertex
	val distance = #2minimum_vertex
	val new_distance = distance + graph_weight(Graph,vertex,a);
	
	val distance_parent_table_a = parent_distance_vertex(parent_table,a)
  in 
	if(distance_parent_table_a = ~1) then
	  let 
		val temp_pt = parent_set_distance_vertex(parent_table,a,new_distance)
		val temp_q = insert_vertex_distance(priority_queue,a,new_distance)
		val temp_pt1 = parent_set_parent_vertex(temp_pt,a,vertex)
	  in 
	   for_all_neighbours(Graph,temp_q,temp_pt1,adjacent_neighbours,minimum_vertex)
	  end
	else
		if(distance_parent_table_a > new_distance) then
		  let 
			val temp_pt = parent_set_distance_vertex(parent_table,a,new_distance)
			val temp_q = set_vertex_distance(priority_queue,a,new_distance)
			val temp_pt1 = parent_set_parent_vertex(temp_pt,a,vertex)
		  in 
		   for_all_neighbours(Graph,temp_q,temp_pt1,adjacent_neighbours,minimum_vertex)
		  end
		else
			(priority_queue, parent_table)
  end ;

  
(*
	Function: 
		Helper function which will execute till priority queue is not empty
	Attributes:
		Graph:
		priority_queue:
		parent_table:
	Results:
		This is the while loop iterative that executes till the priority  queue is not empty
*)
fun priority_loop(Graph, nil, parent_table:(string*int*string)list)= (nil,parent_table)
  | priority_loop(Graph:(string*string*int)list, priority_queue:(string*int)list,parent_table:(string*int*string)list)=
  let 
    val vertex_minimum = minimum_vertex_distance(priority_queue)
	val vertex = #1vertex_minimum
	val distance = #2vertex_minimum
	val new_priority_queue = delete_vertex(priority_queue,vertex)
	val adjacent_neighbours:(string)list = []
	val adjacent_neighbours = list_neighbours(Graph,adjacent_neighbours,vertex)
	val temp = for_all_neighbours(Graph,new_priority_queue,parent_table,adjacent_neighbours,vertex_minimum)
	val new_queue = #1temp;
	val new_parent = #2temp;
	val temp_final = priority_loop(Graph, new_queue,new_parent)
  in 
	temp_final
  end;

(*********************** Dijkstra Function ***********************) 
fun DijkSS(G, s, d) =									(* Get all the children of a given vertex
														   Print the path from the source to the destination	*)
let
	val priority_queue = [] (*  vertex * distance        == Insert vertex and distance in the list
														 == Get the (vertex*distance) from the list
														 == Delete the node of particular vertex from the list.
														 == Update the value of previous vertex from the list.
														 == Check if the vertex exists.
														 *)
	val parent_table = [] (* vertex * distance * parent  == Nothing will be deleted from here.
														 == Get distance for a particular vertex.
														 == Set distance for a particular vertex.
														 == Set Parent for particular vertex.
														 == Check if vertex exists in it.*)
	
	(*Vertex List*)
	val vertex_list = []
	val vertex_list = graph_vertices(G,vertex_list)
	
	(*Initialization of priority table*)
	val parent_table = parent_table_init(parent_table,vertex_list)
	val parent_table = parent_set_distance_vertex(parent_table,s,0)
	
	(*Initialization of priority Queue*)
	val priority_queue =insert_vertex_distance(priority_queue,s,0)
	val full=priority_loop(G, priority_queue,parent_table)
	val str_table = print_parent_table(#2full)
	val path = ""
	val path = print_path(#2full,s,d,"")
in 
    (* Initializing the parent_table*)
    (*parent_table = parent_table_init(parent_table,vertex_list)*)
	(*priority_queue*)
	path
	
end;  
  
(* ************** Test Case 1********************** *)
val G = [("a","b",4),("a","c",1),("b","e",4),("c","b",2),("c","d",4),("d","e",4)];
print "\n\n******************************TestCase 1********************\n\n";
print ("\n"^DijkSS(G, "a", "d")^"\n\n");
print "\n\n****************************** End of TestCase 1*******************\n\n";

(* ************** Test Case 2********************** *)
val G = [("a","b",4),("a","c",1),("b","e",4),("c","b",2),("c","d",4),("d","e",4)];
print "\n\n******************************TestCase 2********************\n\n";
print ("\n"^DijkSS(G, "a", "e")^"\n\n");
print "\n\n****************************** End of TestCase 2********************\n\n";

(* ************** Test Case 3********************** *)
val G = [("a","b",4),("a","c",1),("b","e",4),("c","b",2),("c","d",4),("d","e",4)];
print "\n\n******************************TestCase 3********************\n\n";
print ("\n"^DijkSS(G, "a", "c")^"\n\n");
print "\n\n****************************** End of TestCase 3********************\n\n";

(* ************** Test Case 4********************** *)
val G = [("1","2",2),("1","4",6),("2","4",3),("2","5",10),("3","6",5),("4","3",2),("4","6",8),("4","5",2),("4","7",4),("5","7",6),("7","6",1)];
print "\n\n******************************TestCase 4********************\n\n";
print ("\n"^DijkSS(G, "1", "2")^"\n\n");
print "\n\n****************************** End of TestCase 4********************\n\n";

(* ************** Test Case 5********************** *)
val G = [("1","2",2),("1","4",6),("2","4",3),("2","5",10),("3","6",5),("4","3",2),("4","6",8),("4","5",2),("4","7",4),("5","7",6),("7","6",1)];
print "\n\n******************************TestCase 5********************\n\n";
print ("\n"^DijkSS(G, "1", "3")^"\n\n");
print "\n\n****************************** End of TestCase 5********************\n\n";

(* ************** Test Case 6********************** *)
val G = [("1","2",2),("1","4",6),("2","4",3),("2","5",10),("3","6",5),("4","3",2),("4","6",8),("4","5",2),("4","7",4),("5","7",6),("7","6",1)];
print "\n\n******************************TestCase 6********************\n\n";
print ("\n"^DijkSS(G, "1", "4")^"\n\n");
print "\n\n****************************** End of TestCase 6********************\n\n";

(* ************** Test Case 7********************** *)
val G = [("1","2",2),("1","4",6),("2","4",3),("2","5",10),("3","6",5),("4","3",2),("4","6",8),("4","5",2),("4","7",4),("5","7",6),("7","6",1)];
print "\n\n******************************TestCase 7********************\n\n";
print ("\n"^DijkSS(G, "1", "5")^"\n\n");
print "\n\n****************************** End of TestCase7********************\n\n";
