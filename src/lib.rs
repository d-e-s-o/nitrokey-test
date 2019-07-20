// lib.rs

// *************************************************************************
// * Copyright (C) 2019 Daniel Mueller (deso@posteo.net)                   *
// *                                                                       *
// * This program is free software: you can redistribute it and/or modify  *
// * it under the terms of the GNU General Public License as published by  *
// * the Free Software Foundation, either version 3 of the License, or     *
// * (at your option) any later version.                                   *
// *                                                                       *
// * This program is distributed in the hope that it will be useful,       *
// * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
// * GNU General Public License for more details.                          *
// *                                                                       *
// * You should have received a copy of the GNU General Public License     *
// * along with this program.  If not, see <http://www.gnu.org/licenses/>. *
// *************************************************************************

#![recursion_limit = "128"]
#![warn(
  bad_style,
  dead_code,
  future_incompatible,
  illegal_floating_point_literal_pattern,
  improper_ctypes,
  intra_doc_link_resolution_failure,
  late_bound_lifetime_arguments,
  missing_copy_implementations,
  missing_debug_implementations,
  no_mangle_generic_items,
  non_shorthand_field_patterns,
  nonstandard_style,
  overflowing_literals,
  path_statements,
  patterns_in_fns_without_body,
  plugin_as_library,
  private_in_public,
  proc_macro_derive_resolution_fallback,
  renamed_and_removed_lints,
  rust_2018_compatibility,
  rust_2018_idioms,
  safe_packed_borrows,
  stable_features,
  trivial_bounds,
  trivial_numeric_casts,
  type_alias_bounds,
  tyvar_behind_raw_pointer,
  unconditional_recursion,
  unions_with_drop_fields,
  unreachable_code,
  unreachable_patterns,
  unstable_features,
  unstable_name_collisions,
  unused,
  unused_comparisons,
  unused_import_braces,
  unused_lifetimes,
  unused_qualifications,
  unused_results,
  where_clauses_object_safety,
  while_true
)]

//! A crate providing supporting testing infrastructure for the
//! `nitrokey` crate and its users.
//!
//! The crate simplifies test creation by providing an attribute macro
//! that generates code for running a test on up to two devices (a
//! Nitrokey Pro and Nitrokey Storage) and takes care of serializing all
//! tests tagged with this attribute, and causes them to be skipped if
//! the respective device is not present.
//!
//! It also provides support for running tests belonging to a certain
//! group. There are three groups: "nodev" (representing tests that run
//! when no device is present), "pro" (encompassing tests eligible to
//! run on the Nitrokey Pro), and "storage" (for tests running against a
//! Nitrokey Storage device).
//! Running tests of a specific group (and only those) can be
//! accomplished by setting the `NITROKEY_TEST_GROUP` environment
//! variable to the group of interest. Note that in this mode tests will
//! fail if the respective device is not present.
//!
//! Right now we make a few simplifying assumptions that, although not
//! changing what can be expressed and tested, can lead to unexpected
//! error messages when not known:
//! - the parameter has to be an owned object, not a reference
//! - parameter types are pattern matched against "Storage", "Pro", and
//!   "DeviceWrapper"; that means `use ... as` declarations will not work
//!   properly

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream as Tokens;
use quote::quote;
use quote::TokenStreamExt;
use syn::punctuated;


/// The name of an optional environment variable we honor that can be
/// set to one of the supported groups and will cause only tests of this
/// particular group to be run.
const NITROKEY_TEST_GROUP: &str = "NITROKEY_TEST_GROUP";
/// The name of the group containing tests that run when no device is
/// present.
const NITROKEY_GROUP_NODEV: &str = "nodev";
/// The name of the group containing tests that run when the Nitrokey
/// Pro is present.
const NITROKEY_GROUP_PRO: &str = "pro";
/// The name of the group containing tests that run when the Nitrokey
/// Storage is present.
const NITROKEY_GROUP_STORAGE: &str = "storage";


/// A type used to determine what Nitrokey device to test on.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SupportedDevice {
  /// No device must be present.
  None,
  /// Only the Nitrokey Pro is supported.
  Pro,
  /// Only the Nitrokey Storage is supported.
  Storage,
  /// Both the Nitrokey Pro and Storage are supported.
  Any,
}


/// A type defining what kind of Nitrokey device to connect to and
/// whether to wrap it.
#[derive(Clone, Copy, Debug)]
enum EmittedDevice {
  /// Bail out if the connection succeeds, i.e., only run when no device
  /// is present.
  None,
  /// Connect to and pass in a `nitrokey::Pro`.
  Pro,
  /// Connect to and pass in a `nitrokey::Storage`.
  Storage,
  /// Connect to a `nitrokey::Pro` but pass it in as a `nitrokey::DeviceWrapper`.
  WrappedPro,
  /// Connect to a `nitrokey::Storage` but pass it in as a `nitrokey::DeviceWrapper`.
  WrappedStorage,
}

impl EmittedDevice {
  /// Retrieve the group a device belongs to.
  fn as_group(self) -> DeviceGroup {
    match self {
      EmittedDevice::None => DeviceGroup::No,
      EmittedDevice::Pro |
      EmittedDevice::WrappedPro => DeviceGroup::Pro,
      EmittedDevice::Storage |
      EmittedDevice::WrappedStorage => DeviceGroup::Storage,
    }
  }
}


/// The group a particular device belongs to.
#[derive(Clone, Copy, Debug)]
enum DeviceGroup {
  /// The group encompassing all tests that require no device to be
  /// present.
  No,
  /// The group containing all tests for the Nitrokey Pro.
  Pro,
  /// The group containing all tests for the Nitrokey Storage.
  Storage,
}

impl AsRef<str> for DeviceGroup {
  fn as_ref(&self) -> &str {
    match *self {
      DeviceGroup::No => NITROKEY_GROUP_NODEV,
      DeviceGroup::Pro => NITROKEY_GROUP_PRO,
      DeviceGroup::Storage => NITROKEY_GROUP_STORAGE,
    }
  }
}

impl quote::ToTokens for DeviceGroup {
  fn to_tokens(&self, tokens: &mut Tokens) {
    tokens.append(Literal::string(self.as_ref()))
  }
}


/// A procedural macro for the `test` attribute.
///
/// The attribute can be used to define a test that accepts a Nitrokey
/// device object (which can be any of `nitrokey::Pro`,
/// `nitrokey::Storage`, or `nitrokey::DeviceWrapper`), and runs a test
/// against that device. If the device type was specified as
/// `nitrokey::DeviceWrapper`, the test will actually be invoked for a
/// Nitrokey Pro as well as a Nitrokey Storage. Irrespective, the test
/// is skipped if the device cannot be found.
/// It also supports running tests when no device is present, which is
/// required for tasks such as handling of error conditions. The test
/// function must not accept a device object in that case (i.e., have no
/// parameters).
///
/// # Example
///
/// Test functionality on an arbitrary Nitrokey device (i.e., Pro or
/// Storage):
/// ```rust,no_run
/// // Note that no test would actually run, regardless of `no_run`,
/// // because we do not invoke the function.
/// #[nitrokey_test::test]
/// fn some_nitrokey_test(device: nitrokey::DeviceWrapper) {
///   assert_eq!(device.get_serial_number().unwrap().len(), 8);
/// }
/// ```
///
/// Test functionality on a Nitrokey Pro device:
/// ```rust,no_run
/// #[nitrokey_test::test]
/// fn some_pro_test(device: nitrokey::Pro) {
///   assert_eq!(device.get_model(), nitrokey::Model::Pro);
/// }
/// ```
///
/// Test functionality on a Nitrokey Storage device:
/// ```rust,no_run
/// #[nitrokey_test::test]
/// fn some_storage_test(device: nitrokey::Storage) {
///   assert_eq!(device.get_model(), nitrokey::Model::Storage);
/// }
/// ```
///
/// Test functionality when no device is present:
/// ```rust,no_run
/// #[nitrokey_test::test]
/// fn no_device() {
///   assert!(nitrokey::connect().is_err());
/// }
/// ```
#[proc_macro_attribute]
pub fn test(attr: TokenStream, item: TokenStream) -> TokenStream {
  // Bail out if user tried to pass additional arguments. E.g.,
  // #[nitrokey_test::test(foo = "bar")
  if !attr.is_empty() {
    panic!("unsupported attributes supplied: {}", attr);
  }
  // Make clippy happy.
  drop(attr);

  let input = syn::parse_macro_input!(item as syn::ItemFn);
  let dev_type = determine_device(&input.decl.inputs);

  match dev_type {
    SupportedDevice::None => {
      let name = format!("{}", &input.ident);
      expand_wrapper(name, EmittedDevice::None, &input)
    },
    SupportedDevice::Pro => {
      let name = format!("{}", &input.ident);
      expand_wrapper(name, EmittedDevice::Pro, &input)
    },
    SupportedDevice::Storage => {
      let name = format!("{}", &input.ident);
      expand_wrapper(name, EmittedDevice::Storage, &input)
    },
    SupportedDevice::Any => {
      let name = format!("{}_pro", &input.ident);
      let pro = expand_wrapper(name, EmittedDevice::WrappedPro, &input);

      let name = format!("{}_storage", &input.ident);
      let storage = expand_wrapper(name, EmittedDevice::WrappedStorage, &input);

      // Emit a test for both the Pro and the Storage device.
      quote! {
        #pro
        #storage
      }
    }
  }
  .into()
}

fn expand_connect(group: DeviceGroup, ret_type: &syn::ReturnType) -> Tokens {
  let (ret, check) = match ret_type {
    syn::ReturnType::Default => (quote! { return }, quote! {.unwrap()}),
    // The only two valid return types for a test function are no return
    // value or a Result. We assume a Result<V, E> in this path. Note
    // that we furthermore assume that V=(). Once the trait
    // std::process::Termination stabilized we should be able to do away
    // with the latter assumption.
    syn::ReturnType::Type(_, _) => (quote! { return Ok(()) }, quote! {?}),
  };

  let connect = match group {
    DeviceGroup::No => quote! { manager.connect() },
    DeviceGroup::Pro => quote! { manager.connect_pro() },
    DeviceGroup::Storage => quote! { manager.connect_storage() },
  };

  let connect_cond = if let DeviceGroup::No = group {
    quote! { }
  } else {
    quote! { #connect#check }
  };

  let connect_err = quote! {
    ::nitrokey::Error::CommunicationError(::nitrokey::CommunicationError::NotConnected)
  };
  let skip = if let DeviceGroup::No = group {
    quote! {let Err(#connect_err) = result {} else}
  } else {
    quote! {let Err(#connect_err) = result}
  };

  let result = if let DeviceGroup::No = group {
    quote! { }
  } else {
    quote! { result#check }
  };

  quote! {
    {
      use ::std::io::Write;
      match ::std::env::var(#NITROKEY_TEST_GROUP) {
        Ok(group) => {
          match group.as_ref() {
            #NITROKEY_GROUP_NODEV |
            #NITROKEY_GROUP_PRO |
            #NITROKEY_GROUP_STORAGE => {
              if group == #group {
                #connect_cond
              } else {
                ::std::println!("skipped");
                #ret
              }
            },
            x => ::std::panic!("unsupported {} value: {}", #NITROKEY_TEST_GROUP, x),
          }
        },
        Err(::std::env::VarError::NotUnicode(_)) => {
          ::std::panic!("{} value is not valid unicode", #NITROKEY_TEST_GROUP)
        },
        Err(::std::env::VarError::NotPresent) => {
          // Check if we can connect. Skip the test if we can't due to the
          // device not being present.
          let result = #connect;
          if #skip {
            // Note that tests have a "special" stdout, used by
            // println!() for example, that has a thread-local buffer
            // and is not actually printed by default but only when the
            // --nocapture option is present. Alternatively, they can
            // use std::io::stdout directly, which will always appear.
            // Unfortunately, neither works properly in concurrent
            // contexts. That is, output can always be interleaved
            // randomly. Note that we do serialize tests, but there will
            // always be a window for races, because we have to release
            // the mutex before the "outer" test infrastructure prints
            // the result.
            // For that matter, we use the thread local version to print
            // information about whether a test is skipped. This way,
            // the user can get this information but given that it
            // likely is somewhat garbled we do not want it to manifest
            // by default. This is really a short coming of the Rust
            // testing infrastructure and there is nothing we can do
            // about that. It is a surprise, though, that even the
            // thread-locally buffered version has this problem.
            ::std::println!("skipped");
            #ret
          }
          #result
        },
      }
    }
  }
}

fn expand_arg<P>(device: EmittedDevice, args: &punctuated::Punctuated<syn::FnArg, P>) -> Tokens
where
  P: quote::ToTokens,
{
  // Based on the device we want to emit a test function for we recreate
  // the expected full path of the type. That is necessary because
  // client code may have a "use" and may just contain a `Pro`, for
  // example, while we really need to work with the absolute path.
  let arg_type = match device {
    EmittedDevice::None => quote! {},
    EmittedDevice::Pro => quote! { ::nitrokey::Pro },
    EmittedDevice::Storage => quote! { ::nitrokey::Storage },
    EmittedDevice::WrappedPro |
    EmittedDevice::WrappedStorage => quote! { ::nitrokey::DeviceWrapper },
  };

  match args.first() {
    Some(arg) => match arg.value() {
      syn::FnArg::Captured(captured) => {
        let arg = syn::FnArg::Captured(syn::ArgCaptured {
          pat: captured.pat.clone(),
          colon_token: captured.colon_token,
          ty: syn::Type::Path(syn::parse_quote! { #arg_type }),
        });
        quote! { #arg }
      }
      _ => panic!("unexpected test function argument"),
    },
    None => quote! {},
  }
}

fn expand_call(device: EmittedDevice, wrappee: &syn::ItemFn) -> Tokens {
  let test_name = &wrappee.ident;
  let decl = &wrappee.decl;
  let connect = expand_connect(device.as_group(), &decl.output);

  let call = match device {
    EmittedDevice::None => quote! { #test_name() },
    EmittedDevice::Pro |
    EmittedDevice::Storage => quote! { #test_name(device) },
    EmittedDevice::WrappedPro => {
      quote! {
        #test_name(::nitrokey::DeviceWrapper::Pro(device))
      }
    },
    EmittedDevice::WrappedStorage => {
      quote! {
        #test_name(::nitrokey::DeviceWrapper::Storage(device))
      }
    },
  };

  if let EmittedDevice::None = device {
    // Make sure that if no device is passed in the user is still
    // allowed to use nitrokey::take successfully by not keeping a
    // Manager object lying around. We just need it to check whether or
    // not to skip the test.
    quote! {
      {
        let mut manager = ::nitrokey::force_take().unwrap();
        let _ = #connect;
      }
      #call
    }
  } else {
    quote! {
      let mut manager = ::nitrokey::force_take().unwrap();
      let device = #connect;
      #call
    }
  }
}

/// Emit code for a wrapper function around a Nitrokey test function.
fn expand_wrapper<S>(fn_name: S, device: EmittedDevice, wrappee: &syn::ItemFn) -> Tokens
where
  S: AsRef<str>,
{
  // Note that we need to rely on proc_macro2 here, because while the
  // compiler provided proc_macro has `Ident` and `Span` types, they
  // cannot be interpolated with quote!{} for lack of quote::ToTokens
  // implementations.
  let name = Ident::new(fn_name.as_ref(), Span::call_site());
  let attrs = &wrappee.attrs;
  let decl = &wrappee.decl;
  let body = &wrappee.block;
  let test_name = &wrappee.ident;
  let test_arg = expand_arg(device, &decl.inputs);
  let test_call = expand_call(device, wrappee);

  let ret_type = match &decl.output {
    syn::ReturnType::Default => quote! {()},
    syn::ReturnType::Type(_, type_) => quote! {#type_},
  };

  quote! {
    #[test]
    #(#attrs)*
    fn #name() -> #ret_type {
      fn #test_name(#test_arg) -> #ret_type {
        #body
      }

      // Note that mutexes (and other locks) come with a poisoning
      // mechanism that (by default) prevents an acquisition of a mutex
      // that was held while the thread holding it panic'ed. We don't
      // care about that protection. There are no real invariants that
      // our mutex is protecting, it just synchronizes accesses to the
      // nitrokey device. As such, just override the protection.
      let _guard = ::nitrokey_test_state::mutex()
        .lock()
        .map_err(|err| err.into_inner());
      #test_call
    }
  }
}

fn determine_device_for_arg(arg: &syn::FnArg) -> SupportedDevice {
  match arg {
    syn::FnArg::Captured(arg) => {
      let type_ = &arg.ty;
      match type_ {
        syn::Type::Path(path) => {
          if path.path.segments.is_empty() {
            panic!("invalid function argument type: {}", quote! {#path});
          }

          let type_ = format!("{}", path.path.segments.last().unwrap().value().ident);
          match type_.as_ref() {
            "Storage" => SupportedDevice::Storage,
            "Pro" => SupportedDevice::Pro,
            "DeviceWrapper" => SupportedDevice::Any,
            _ => panic!("unsupported function argument type: {}", type_),
          }
        },
        _ => panic!("unexpected function argument type: {} (expected owned object)",
                    quote!{#type_}),
      }
    }
    _ => panic!("unexpected function argument signature: {}", quote! {#arg}),
  }
}

/// Determine the kind of Nitrokey device a test function support, based
/// on the type of its only parameter.
fn determine_device<P>(args: &punctuated::Punctuated<syn::FnArg, P>) -> SupportedDevice
where
  P: quote::ToTokens,
{
  match args.len() {
    0 => SupportedDevice::None,
    1 => determine_device_for_arg(&args[0]),
    _ => panic!("functions used as Nitrokey tests can only have zero or one argument"),
  }
}


#[cfg(test)]
mod tests {
  use super::determine_device;
  use super::SupportedDevice;

  use syn;


  #[test]
  fn determine_nitrokey_none() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_none() {}
    };
    let dev_type = determine_device(&input.decl.inputs);

    assert_eq!(dev_type, SupportedDevice::None)
  }

  #[test]
  fn determine_nitrokey_pro() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_pro(device: nitrokey::Pro) {}
    };
    let dev_type = determine_device(&input.decl.inputs);

    assert_eq!(dev_type, SupportedDevice::Pro)
  }

  #[test]
  fn determine_nitrokey_storage() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_storage(device: nitrokey::Storage) {}
    };
    let dev_type = determine_device(&input.decl.inputs);

    assert_eq!(dev_type, SupportedDevice::Storage)
  }

  #[test]
  fn determine_any_nitrokey() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_any(device: nitrokey::DeviceWrapper) {}
    };
    let dev_type = determine_device(&input.decl.inputs);

    assert_eq!(dev_type, SupportedDevice::Any)
  }

  #[test]
  #[should_panic(expected = "functions used as Nitrokey tests can only have zero or one argument")]
  fn determine_wrong_argument_count() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_pro(device: nitrokey::Pro, _: i32) {}
    };
    let _ = determine_device(&input.decl.inputs);
  }

  #[test]
  #[should_panic(expected = "unexpected function argument signature: & self")]
  fn determine_wrong_function_type() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_self(&self) {}
    };
    let _ = determine_device(&input.decl.inputs);
  }

  #[test]
  #[should_panic(expected = "unexpected function argument type: & nitrokey \
                             :: DeviceWrapper (expected owned object)")]
  fn determine_wrong_argument_type() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_any(device: &nitrokey::DeviceWrapper) {}
    };
    let _ = determine_device(&input.decl.inputs);
  }

  #[test]
  #[should_panic(expected = "unsupported function argument type: FooBarBaz")]
  fn determine_invalid_argument_type() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_foobarbaz(device: nitrokey::FooBarBaz) {}
    };
    let _ = determine_device(&input.decl.inputs);
  }
}
