From Stdlib Require Export Strings.String.
From Stdlib Require Import List.
Import ListNotations.
From SECD Require Import Maps.
Inductive AE : Type :=
  | Identifier (s: string)
  | Lambda (x: string) (body: AE)
  | Combination (rator: AE) (rand: AE)
.
Inductive Control : Type :=
  | Ae (ae: AE)
  | Ap
.

Inductive Stack :=
  | closure (e: partial_map Stack) (x: string) (c: Control)
  | ae (ae : AE)
.
Definition Environment := partial_map Stack.

Inductive State :=
  | state (S: list Stack) (E: Environment) (C: list Control) (D: State)
  | empt
.
Definition Transform (st : State) : State :=
  match st with
    | state s e c d =>
        match c with
          | [] =>
              match d with
                | empt =>
                    match s with
                       | a :: _ => state [a] empty [] empt
                       | [] => state [] empty [] empt
                    end
                | state s' e' c' d' =>
                    match s with
                       | a :: _ => state (a :: s') e' c' d'
                       | [] => state s' e' c' d'
                    end
              end
          | x :: c =>
              match x with
                | Ae a =>
                    match a with
                      | Identifier var =>
                          match e var with
                            | Some v => state (v :: s) e c d
                            | None => empt
                          end
                      | Lambda bv body =>
                          state ((closure e bv (Ae body)) :: s) e c d
                      | Combination rand rator =>
                          state s e ((Ae rand) :: (Ae rator) :: Ap :: c) d
                    end
                | Ap =>
                    match s with
                      | a :: s' =>
                          match a with
                            | closure e' j c' =>
                                match s' with
                                  | b :: s'' =>
                                      state [] (j |-> b; e') [c']
                                        (state s'' e c d)
                                  | [] => empt (* TODO: this shouldn't happen *)
                                end
                            | ae _ => empt
                          end
                      | [] => empt (* TODO: proof that this will never happen *)
                    end
              end
        end
    | empt => empt
  end.

From Stdlib Require Extraction.
From Stdlib Require Import ExtrHaskellBasic.
From Stdlib Require Import ExtrHaskellString.
Set Extraction Output Directory ".".
Extraction Language Haskell.
Extraction "SECD.hs" Transform.
