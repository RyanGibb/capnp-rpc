module S = S
module RO_array = RO_array
module Stats = Stats
module Id = Id
module Debug = Debug
module Error = Error
module Exception = Exception
module Message_types = Message_types
module Core_types (W : S.WIRE) : S.CORE_TYPES with module Wire = W
module Local_struct_promise = Local_struct_promise
module Cap_proxy = Cap_proxy
module CapTP = CapTP
module RC = RC
