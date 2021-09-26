(**

   Chers programmeuses et programmeurs de λman, votre mission consiste
   à compléter ce module pour faire de vos λmen les meilleurs robots
   de la galaxie. C'est d'ailleurs le seul module qui vous pouvez
   modifier pour mener à bien votre mission.

   La fonction à programmer est

         [decide : memory -> observation -> action * memory]

   Elle est appelée à chaque unité de temps par le Λserver avec de
   nouvelles observations sur son environnement. En réponse à ces
   observations, cette fonction décide quelle action doit effectuer le
   robot.

   L'état du robot est représenté par une valeur de type [memory].  La
   fonction [decide] l'attend en argument et renvoie une nouvelle
   version de cette mémoire. Cette nouvelle version sera passée en
   argument à [decide] lors du prochain appel.

*)

open World
open Space

(** Le Λserver transmet les observations suivantes au λman: *)
type observation = World.observation

(** Votre λman peut se déplacer : il a une direction D et une vitesse V.

    La direction est en radian dans le repère trigonométrique standard :

    - si D = 0. alors le robot pointe vers l'est.
    - si D = Float.pi /. 2.  alors le robot pointe vers le nord.
    - si D = Float.pi alors le robot pointe vers l'ouest.
    - si D = 3 * Float.pi / 2 alors le robot pointe vers le sud.
      (Bien entendu, ces égalités sont à lire "modulo 2 * Float.pi".)

    Son déplacement en abscisse est donc V * cos D * dt et en ordonnée
    V * sin D * dt.

    Votre λman peut communiquer : il peut laisser du microcode sur sa
    position courante pour que d'autres λmen puissent le lire.  Un
    microcode s'autodétruit au bout d'un certain nombre d'unités de
    temps mais si un microcode est laissé près d'un autre microcode
    identique, ils fusionnent en un unique microcode dont la durée de
    vie est le somme des durées de vie des deux microcodes initiaux.
    Construire un microcode demande de l'énergie au robot : chaque
    atome lui coûte 1 point d'énergie. Heureusement, l'énergie augmente
    d'1 point toutes les unités de temps.

    Pour terminer, votre λman peut couper des arbres de Böhm. Les
    arbres de Böhm ont un nombre de branches variables. Couper une
    branche prend une unité de temps et augmente le score de 1
    point. Si on ramène cette branche au vaisseau, un second point est
    accordé.

    Pour finir, le monde est malheureusement très dangereux : on y
    trouve des bouches de l'enfer dans lesquelles il ne faut pas tomber
    ainsi que des champs de souffrances où la vitesse de votre robot
    est modifiée (de -50% à +50%).

*)

type action =
  | Move of Space.angle * Space.speed
  (** [Move (a, v)] est l'angle et la vitesse souhaités pour la
      prochaine unité de temps. La vitesse ne peut pas être négative et
      elle ne peut excéder la vitesse communiquée par le serveur. *)

  | Put of microcode * Space.duration
  (** [Put (microcode, duration)] pose un [microcode] à la position courante
      du robot. Ce microcode s'autodétruira au bout de [duration] unité de
      temps. Par contre, s'il se trouve à une distance inférieure à
      [Space.small_distance] d'un microcode similaire, il est fusionné
      avec ce dernier et la durée de vie du microcode résultant est
      la somme des durées de vide des deux microcodes. *)

  | ChopTree
  (** [ChopTree] coupe une branche d'un arbre de Böhm situé une distance
      inférieure à [Space.small_distance] du robot. Cela augmente le score
      de 1 point. *)

  | Wait
  (** [Wait] ne change rien jusqu'au prochain appel. *)

  | Die of string
  (** [Die] est produit par les robots dont on a perdu le signal. *)

[@@deriving yojson]

(**

   Le problème principal de ce projet est le calcul de chemin.

   On se dote donc d'un type pour décrire un chemin : c'est une
   liste de positions dont la première est la source du chemin
   et la dernière est sa cible.

*)
type path = Space.position list

(** Version lisible des chemins. *)
let string_of_path path =
  String.concat " " (List.map string_of_position path)

(**

   Nous vous proposons de structurer le comportement du robot
   à l'aide d'objectifs décrits par le type suivant :

*)
type objective =
  | Initializing            (** Le robot doit s'initialiser.       *)
  | Chopping                (** Le robot doit couper des branches. *)
  | GoingTo of path * path
  (** Le robot suit un chemin. Le premier chemin est la liste des
      positions restantes tandis que le second est le chemin initial.
      On a donc que le premier chemin est un suffixe du second. *)

(** Version affichable des objectifs. *)
let string_of_objective = function
  | Initializing -> "initializing"
  | Chopping -> "chopping"
  | GoingTo (path, _) ->
    Printf.sprintf
      "going to %s" (String.concat " " (List.map string_of_position path))

let findangle p1 p2 =
  match p1,p2 with
  | (x1,y1),(x2,y2) -> angle_of_float (Float.pi +. atan2 (y2-.y1) (x2-.x1))

(**

   Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

   On vous propose de structurer la mémoire comme suit:

*)
type memory = {
  known_world : World.t option;      (** Le monde connu par le robot.     *)
  graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
  objective   : objective;           (** L'objectif courant du robot.     *)
  targets     : Space.position list; (** Les points où il doit se rendre. *)
}

(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
  known_world = None;
  graph       = Graph.empty;
  objective   = Initializing;
  targets     = [];
}

(**

   Traditionnellement, la fonction de prise de décision d'un robot
   est la composée de trois fonctions :

   1. "discover" qui agrège les observations avec les observations
      déjà faites dans le passé.

   2. "plan" qui met à jour le plan courant du robot en réaction
      aux nouvelles observations.

   3. "next_action" qui décide qu'elle est l'action à effectuer
       immédiatement pour suivre le plan.

*)

(** [discover] met à jour [memory] en prenant en compte les nouvelles
    observations. *)
let discover visualize observation memory =
  let seen_world = World.world_of_observation observation in
  let known_world =
    match memory.known_world with
    | None -> seen_world
    | Some known_world -> World.extends_world known_world seen_world
  in
  if visualize then Visualizer.show ~force:true known_world;
  { memory with known_world = Some known_world }


(**

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

*)

let float_of_dist dist =
  match dist with
  | Distance d -> d



let x_1 bounding_box =
  fst (fst bounding_box)

let y_1 bounding_box =
  snd (fst bounding_box)

let x_2 bounding_box =
  fst (snd bounding_box)

let y_2 bounding_box =
  snd (snd bounding_box)

(*let tree_score robot_pos tree =
  let dist = float_of_dist (dist2 tree.tree_position robot_pos) in 
  let branches = float_of_int tree.branches in
  branches - 2.5*.dist *)



let rec closest_tree tree_list robot_pos spaceship_pos =
  match tree_list with
  | [] -> spaceship_pos
  | h :: [] -> h.tree_position
  (*Seul contre la montre : on fait le tour des arbres de proche en proche sans viser ceux avec le plus de branches *)
  | h :: t -> closest_tree ((List.filter (fun tree -> (dist2 tree.tree_position robot_pos) < (dist2 h.tree_position robot_pos)) t)@[h]) robot_pos spaceship_pos
  (* En bataille : on laisse aux nazes les arbres peu feuillus pour viser les meilleurs arbres (mais pas trop loin non plus)
  | h :: t -> closest_tree 
    (
      ( 
        List.filter 
        (
          fun tree ->  ( tree_score robot_pos tree) > ( tree_score robot_pos h)
        ) 
        t 
      ) 
      @[h]
    ) 
    robot_pos 
    spaceship_pos*)
 

let colinear_segment s1 s2 =
  let x1A = x_1 s1 in
  let y1A = y_1 s1 in
  let x1B = x_2 s1 in
  let y1B = y_2 s1 in
  let leading_coeff_s1 = (y1B-.y1A)/.(x1B-.x1A) in
  let x2A = x_1 s2 in
  let y2A = y_1 s2 in
  let x2B = x_2 s2 in
  let y2B = y_2 s2 in
  let leading_coeff_s2 = (y2B-.y2A)/.(x2B-.x2A) in
  leading_coeff_s1=leading_coeff_s2

let segments_intersection s1 s2 =
  let (+) = (+.) in
  let (-) = (-.) in
  let (/) = (/.) in
  let ( * ) = ( *. ) in
  let x1 = x_1 s1 in
  let y1 = y_1 s1 in
  let x2 = x_2 s1 in
  let y2 = y_2 s1 in
  let x3 = x_1 s2 in
  let y3 = y_1 s2 in
  let x4 = x_2 s2 in
  let y4 = y_2 s2 in
  let num = (y3-y4)*(x1-x3)+(x4-x3)*(y1-y3) in
  let den = (x4-x3)*(y1-y2)-(x1-x2)*(y4-y3) in
    let t = num/den in
    ((x1+t*(x2-x1)),(y1+t*(y2-y1)))


let extend bounding_box_ =
  let x1 = x_1 bounding_box_ -. 1. in
  let y1 = y_1 bounding_box_ -. 1. in
  let x2 = x_2 bounding_box_ +. 1. in
  let y2 = y_2 bounding_box_ +. 1. in
  ((x1,y1),(x2,y2))

let extended_bounding_list observation =
  let hell_list = polygons observation.around (fun _ -> true)
  in let bounding_hell_list = List.map
         ( fun polygone -> bounding_box_of_positions (vertices polygone))
         hell_list
  in let extended_bounding_list = List.map
         (fun bounding_box -> extend bounding_box) bounding_hell_list
  in List.map (fun bounding_box ->
             let x1 = x_1 bounding_box in
             let x2 = x_2 bounding_box in
             let y1 = y_1 bounding_box in
             let y2 = y_2 bounding_box in
             let center = (((x1+.x2)/.2.),((y1+.y2)/.2.)) in
             rectangle center (x2-.x1) (y2-. y1) (Ground(1.))) extended_bounding_list

let rec not_intersecting_hell segment segments_hell =
  match segments_hell with
  | [] -> true
  | h::t -> if (segment_intersects segment h) then false else not_intersecting_hell segment t

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | (a,b) :: t ->
    if ( a==b || (List.mem (a,b) l2) || (List.mem (b,a) l2))
    then union t l2
    else union t (l2@[(a,b)])

let different_from_any_segment list (a,b) =
  not (List.mem (a,b) list || List.mem (b,a) list)

let rec node_to_edge couple_node edge_liste scalar=
  match couple_node with
  | [] -> edge_liste
  | h :: t -> node_to_edge t ([(fst h, snd h, (float_of_dist ((dist2 (fst h) (snd h))))/.scalar)]@edge_liste) scalar

let build_edge_list node_list hell_list suff_list =
  let couple_node = union (List.flatten (List.map (fun x -> List.map (fun y -> (x,y)) node_list) node_list)) [] in
  let couple_node = List.filter (fun segment -> not_intersecting_hell segment hell_list && different_from_any_segment suff_list segment) couple_node in
  node_to_edge couple_node [] 1.

let scalar suffer_poly =
  match Space.content suffer_poly with
  | Ground f -> f
  | _ -> 0.

let vertices_and_intersection_points polygone segment =
  let vert = vertices polygone in
  let poly_segs = polygon_segments polygone in
  let poly_segs_inter = List.filter ( fun seg -> segment_intersects seg segment && not (colinear_segment segment seg ) ) poly_segs in
  let inter_points = List.map ( fun seg -> segments_intersection seg segment ) poly_segs_inter in
  List.append vert inter_points

let rec intra_suffer_edges list hell_list =
  match list with
  | [] -> []
  | h :: t ->
    let l = fst h in
    let scal = snd h in
    let couple_node = union (List.flatten (List.map (fun x -> List.map (fun y -> (x,y)) l) l)) [] in
    let couple_node = List.filter (fun segment -> not_intersecting_hell segment hell_list) couple_node in
    List.append (node_to_edge couple_node [] scal) (intra_suffer_edges t hell_list)


let visibility_graph observation memory =
    let robot_pos = observation.position in
      match memory.objective with
      | Initializing ->
        let node_list = (tree_positions observation.trees) in
        let closest_tree = closest_tree observation.trees robot_pos observation.spaceship in
        let node_list = node_list@[robot_pos]@[observation.spaceship] in
        let edge_list = [(robot_pos, closest_tree, float_of_dist (dist2 robot_pos closest_tree))]
        in Graph.make node_list edge_list
      | GoingTo (a,b) ->
        let path_segment = (robot_pos, (List.hd b)) in
        let hell_poly_list = polygons observation.around (fun poly -> poly = Hell) in
        let hell_segments_list = List.flatten (List.map ( fun polygone -> polygon_segments polygone ) hell_poly_list) in


        let suffer_poly_list = polygons observation.around (fun poly -> poly != Hell) in
        let suffer_node_list = List.flatten (List.map ( fun polygone -> vertices_and_intersection_points polygone path_segment) suffer_poly_list ) in
        let suffer_nodescalar_list = List.map (fun polygone -> ((vertices_and_intersection_points polygone path_segment), (scalar polygone))) suffer_poly_list in
        let suffer_edges = intra_suffer_edges suffer_nodescalar_list hell_segments_list in
        let suffer_segments_list = List.map ( fun (a,b,_) -> (a,b) ) suffer_edges in



        let suffer_segments_list_bis = List.flatten
        (List.map (fun (d,e,f) ->
          List.filter (fun (a,b,c) ->
            segment_intersects (a,b) (d,e) && not (colinear_segment (a,b) (d,e)))
          suffer_edges)
        suffer_edges) in

        let suffer_segments_point = List.flatten (List.map (fun (d,e,f) -> List.map (fun (a,b,c) -> ([segments_intersection (a,b) (d,e)],c)) suffer_segments_list_bis) suffer_segments_list_bis )in
        let suffer_edges = intra_suffer_edges suffer_segments_point hell_segments_list in
        
        

        let extended_rectangle_list = extended_bounding_list observation in
        let rectangle_node_list = List.flatten (List.map ( fun polygone -> vertices_and_intersection_points polygone path_segment) extended_rectangle_list ) in
        let node_list = [robot_pos]@[List.hd b]@suffer_node_list@rectangle_node_list in
        let edge_list = build_edge_list node_list hell_segments_list suffer_segments_list in
        let edge_list = List.append edge_list suffer_edges in
        Graph.make node_list edge_list

      | Chopping -> memory.graph


(**

   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.

*)

module Node = struct
  type t = Graph.node
  let compare n1 n2 =
    match n1,n2 with
    | (x1,y1),(x2,y2) ->
      if (x1=x2)&&(y1=y2) then 0
      else if ( x1 > x2 ) then 1
      else if ( x2 > x1 ) then -1
      else if ( y1 > y2 ) then 1
      else -1
end
module PQ = PriorityQueue.Make (Node) (Float)

let fst_3 (a,_,_) = a
let snd_3 (_,b,_) = b
let trd_3 (_,_,c) = c


let shortest_path graph source target =
  let pq = PQ.empty in
  let pq = PQ.insert pq source 0. in
  let neighbours n =
    let neighbouring_edges = Graph.out graph n in
    List.map
      (function (a,b,c) -> if (a=n) then (b,c) else (a,c))
      neighbouring_edges
  in
  let rec update pq n neigh_n list_pred visited =
    match neigh_n with
    | [] -> (PQ.remove_min pq,list_pred,visited)
    | (node,dist) :: t ->
      let dist_from_n = (PQ.priority pq n) +. dist  in
      match (List.assoc_opt node list_pred) with
      | None ->
        let pq = PQ.insert pq node dist_from_n in
        let list_pred = List.append list_pred [(node,n)] in
        update pq n t list_pred visited
      | Some _ ->
        if ((not (List.mem node visited)) && PQ.priority pq node > dist_from_n)
        then
          let pq = PQ.decrease pq node dist_from_n in
          let list_pred = (List.remove_assoc node list_pred)@[(node,n)] in
          update pq n t list_pred visited
        else
          update pq n t list_pred visited
  in
  let rec dijkstra triplet_pq_list_pred_visited =
    let pq = fst_3 triplet_pq_list_pred_visited in
    let list_pred = snd_3 triplet_pq_list_pred_visited in
    let visited = trd_3 triplet_pq_list_pred_visited in
    match PQ.get_min pq with
    | None -> list_pred
    | Some (_,n) ->
      let visited = n::visited in
      let neighbours_n = neighbours n in
      dijkstra (update pq n neighbours_n list_pred visited)
  in
  let rec path_to a list_pred =
    match a with
    | n when n=source -> []
    | n -> n::(path_to (List.assoc n list_pred) list_pred)
  in
  List.rev (path_to target (dijkstra (pq,[(source,source)],[])))

(**

   [plan] doit mettre à jour la mémoire en fonction de l'objectif
   courant du robot.

   Si le robot est en train de récolter du bois, il n'y a rien à faire
   qu'attendre qu'il ait fini.

   Si le robot est en train de suivre un chemin, il faut vérifier que
   ce chemin est encore valide compte tenu des nouvelles observations
   faites, et le recalculer si jamais ce n'est pas le cas.

   Si le robot est en phase d'initialisation, il faut fixer ses cibles
   et le faire suivre un premier chemin.

*)

let next_target observation =
  let robot_pos = observation.position in
  let spaceship = observation.spaceship in
  let tree_list = List.filter ( fun tree -> tree.branches > 0 ) observation.trees in
  let closest_tree = closest_tree tree_list robot_pos spaceship in
  GoingTo ([closest_tree],[closest_tree])

let keep_chopping_or_leave observation memory =
  match (tree_at observation.trees observation.position) with
  | None -> memory.objective
  | Some tree ->
    match tree.branches with
    | 0 -> next_target observation
    | _ -> Chopping

let close_to_a_tree observation memory target =
  let current_pos = observation.position in
  let angle = findangle target current_pos in
  let speed = observation.max_speed in
  match tree_at observation.trees current_pos with
  | None ->
    ((Move (angle , speed)), memory)
  | Some tree ->
    match tree.branches with
    | a when a < 1 -> ((Move(angle,speed)), memory )
    | _ -> ((Move (angle_of_float 0.,speed_of_float 0.)),{memory with objective = Chopping })


let plan visualize observation memory =
  Visualizer.show_graph memory.graph ;
  let memory = { memory with graph = visibility_graph observation memory} in
  let ifchopping memory = keep_chopping_or_leave observation memory in
  let new_objective = match memory.objective with
    | Chopping -> ifchopping memory
    | Initializing -> next_target observation
    | GoingTo (a,b) ->
        (* GoingTo (a,b) *)
      let path = shortest_path memory.graph observation.position (List.hd b) in GoingTo(path,b)
  in

  {memory with objective = new_objective}



(**

   Next action doit choisir quelle action effectuer immédiatement en
   fonction de l'objectif courant.

   Si l'objectif est de s'initialiser, la plannification a mal fait
   son travail car c'est son rôle d'initialiser le robot et de lui
   donner un nouvel objectif.

   Si l'objectif est de couper du bois, coupons du bois! Si on vient
   de couper la dernière branche, alors il faut changer d'objectif
   pour se déplacer vers une autre cible.

   Si l'objectif est de suivre un chemin, il faut s'assurer que
   la vitesse et la direction du robot sont correctes.

*)



let next_action visualize observation memory =
 
  match memory.objective with
  | GoingTo (a,b) -> (close_to_a_tree observation memory (List.hd a))
  | Chopping -> (ChopTree,memory)
  | Initializing -> (Wait,memory)


(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)
let decide visualize observation memory : action * memory =
  let memory = discover visualize observation memory in
  let memory = plan visualize observation memory in
  next_action visualize observation memory
