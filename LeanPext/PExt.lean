import Lean
open Lean

namespace PExt

abbrev NatDBExtension :=
  PersistentEnvExtension (Name × Array Nat) (Name × Array Nat) (HashMap Name (Array Nat))

initialize natDBExt : NatDBExtension ← registerPersistentEnvExtension {
  name            := `NatDBExt
  mkInitial       := pure {}
  addEntryFn      := fun s n => s.insert n.1 n.2
  -- **Note** We suppose that, if module `a` imports module `b`,
  --   then the index of `a` within the `arr` is greater than the index of `b` in `arr`
  addImportedFn   := fun arr => pure <| HashMap.ofList (arr.concatMap id).toList,
  exportEntriesFn := fun s => s.toArray
}

syntax (name := declarenatdb) "#declare_natdb" ident : command
syntax (name := printnatdb) "#print_natdb" ident : command
syntax (name := addnat) "#add_nat" ident "[" num,* "]" : command

open Elab Command

def findNatDB (dbname : Name) : CoreM (Option (Array Nat)) := do
  let dbname := dbname
  let state := natDBExt.getState (← getEnv)
  if let some db := state.find? dbname then
    return .some db
  else
    return .none

@[command_elab declarenatdb]
def elabdeclarenatdb : CommandElab := fun stx => do
  match stx with
  | `(declarenatdb | #declare_natdb $dbname) =>
    let dbname := dbname.getId
    let state := natDBExt.getState (← getEnv)
    if let some db := state.find? dbname then
      throwError "Database {repr db} has already been declared"
    else
      let state' := state.insert dbname .empty
      modifyEnv fun env => natDBExt.modifyState env fun _ => state'
  | _ => throwUnsupportedSyntax

@[command_elab printnatdb]
def elabprintnatdb : CommandElab := fun stx => do
  match stx with
  | `(printnatdb | #print_natdb $dbname) =>
    let .some db ← liftCoreM <| findNatDB dbname.getId
      | liftCoreM <| throwError "Not found"
    logInfoAt stx m!"{db}"
  | _ => throwUnsupportedSyntax

@[command_elab addnat]
def elabaddnat : CommandElab := fun stx => do
  match stx with
  | `(addnat | #add_nat $db [$[$dbs],*]) =>
    let db := db.getId
    let dbs := dbs.map (fun x => x.getNat)
    let state := natDBExt.getState (← getEnv)
    if let some dba := state.find? db then
      let state' := state.insert db (dba ++ dbs)
      modifyEnv fun env => natDBExt.modifyState env fun _ => state'
    else
      throwError "Database {repr db} has already been declared"
  | _ => throwUnsupportedSyntax

end PExt
