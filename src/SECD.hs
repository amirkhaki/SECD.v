module SECD where

import qualified Prelude

type Total_map a = Prelude.String -> a

t_empty :: a1 -> Total_map a1
t_empty v _ =
  v

t_update :: (Total_map a1) -> Prelude.String -> a1 -> Prelude.String -> a1
t_update m x v x' =
  case ((Prelude.==) :: Prelude.String -> Prelude.String -> Prelude.Bool) x x' of {
   Prelude.True -> v;
   Prelude.False -> m x'}

type Partial_map a = Total_map (Prelude.Maybe a)

empty :: Partial_map a1
empty =
  t_empty Prelude.Nothing

update :: (Partial_map a1) -> Prelude.String -> a1 -> Prelude.String -> Prelude.Maybe
          a1
update m x v =
  t_update m x (Prelude.Just v)

data AE =
   Identifier Prelude.String
 | Lambda Prelude.String AE
 | Combination AE AE

data Control =
   Ae AE
 | Ap

data Stack =
   Closure (Partial_map Stack) Prelude.String Control
 | Ae0 AE

type Environment = Partial_map Stack

data State =
   State0 (([]) Stack) Environment (([]) Control) State
 | Empt

transform :: State -> State
transform st =
  case st of {
   State0 s e c d ->
    case c of {
     ([]) ->
      case d of {
       State0 s' e' c' d' ->
        case s of {
         ([]) -> State0 s' e' c' d';
         (:) a _ -> State0 ((:) a s') e' c' d'};
       Empt ->
        case s of {
         ([]) -> State0 ([]) empty ([]) Empt;
         (:) a _ -> State0 ((:) a ([])) empty ([]) Empt}};
     (:) x c0 ->
      case x of {
       Ae a ->
        case a of {
         Identifier var ->
          case e var of {
           Prelude.Just v -> State0 ((:) v s) e c0 d;
           Prelude.Nothing -> Empt};
         Lambda bv body -> State0 ((:) (Closure e bv (Ae body)) s) e c0 d;
         Combination rand rator -> State0 s e ((:) (Ae rand) ((:) (Ae rator) ((:) Ap
          c0))) d};
       Ap ->
        case s of {
         ([]) -> Empt;
         (:) a s' ->
          case a of {
           Closure e' j c' ->
            case s' of {
             ([]) -> Empt;
             (:) b s'' -> State0 ([]) (update e' j b) ((:) c' ([])) (State0 s'' e c0
              d)};
           Ae0 _ -> Empt}}}};
   Empt -> Empt}

