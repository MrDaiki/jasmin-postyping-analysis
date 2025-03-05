open Jasmin

module Arch =
  ( val let use_set0 = true and use_lea = false in
        let call_conv = Glob_options.Linux in
        let module C : Arch_full.Core_arch =
          (val CoreArchFactory.core_arch_x86 ~use_lea ~use_set0 call_conv)
        in
        (module Arch_full.Arch_from_Core_arch (C) : Arch_full.Arch) )
