
class virtual ['subject, 'event] observer =
   object
     method virtual notify : 'subject ->  'event -> unit
   end

class ['event] subject =
   object (self : 'subject)
     val mutable observers = ([]: (('subject, 'event) observer) list)
     method add_observer obs = observers <- (obs :: observers)
     method notify_observers (e : 'event) =
         List.iter (fun x -> x#notify self e) observers
   end

type id = int

class entity (id : id) =
  object
    val ent_destroy_subject = new subject
    method destroy_subject : (id) subject = ent_destroy_subject

    method entity_id = id
  end

class ['entity] entity_container =
  object (self)
    inherit ['entity, id] observer as observer

    method add_entity (e : 'entity) =
      e#destroy_subject#add_observer (self)

    method notify _ id = ()
  end

let f (x : entity entity_container) = ()

(*
class world =
  object
    val entity_container : entity entity_container = new entity_container

    method add_entity (s : entity) =
      entity_container#add_entity (s :> entity)

  end
*)
