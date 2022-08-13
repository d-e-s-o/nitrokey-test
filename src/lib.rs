// lib.rs

// *************************************************************************
// * Copyright (C) 2019-2022 Daniel Mueller (deso@posteo.net)              *
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
  broken_intra_doc_links,
  dead_code,
  future_incompatible,
  illegal_floating_point_literal_pattern,
  improper_ctypes,
  late_bound_lifetime_arguments,
  missing_copy_implementations,
  missing_debug_implementations,
  no_mangle_generic_items,
  non_shorthand_field_patterns,
  nonstandard_style,
  overflowing_literals,
  path_statements,
  patterns_in_fns_without_body,
  private_in_public,
  proc_macro_derive_resolution_fallback,
  renamed_and_removed_lints,
  rust_2018_compatibility,
  rust_2018_idioms,
  stable_features,
  trivial_bounds,
  trivial_numeric_casts,
  type_alias_bounds,
  tyvar_behind_raw_pointer,
  unaligned_references,
  unconditional_recursion,
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
//! that generates code for running a test on up to three devices (
//! Nitrokey Pro, Nitrokey Storage, and Librem Key), takes care of
//! serializing all tests tagged with this attribute, and causes them to
//! be skipped if the respective device is not present.
//!
//! It also provides support for running tests belonging to a certain
//! group. There are four groups: "nodev" (representing tests that run
//! when no device is present), "librem" (comprised of all tests that
//! can run on the Librem Key), "pro" (encompassing tests eligible to
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
/// The name of the group containing tests that run when the Librem Key
/// is present.
const NITROKEY_GROUP_LIBREM: &str = "librem";
/// The name of the group containing tests that run when the Nitrokey
/// Pro is present.
const NITROKEY_GROUP_PRO: &str = "pro";
/// The name of the group containing tests that run when the Nitrokey
/// Storage is present.
const NITROKEY_GROUP_STORAGE: &str = "storage";


/// The kind of argument a test function accepts.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ArgumentType {
  /// Pass an actual device handle to the test function.
  Device,
  /// Pass in a device wrapper to the test function.
  DeviceWrapper,
  /// Pass a `Model` object to the test function.
  Model,
}

/// A type used to determine what Nitrokey device to test on.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SupportedDevice {
  /// Only the Librem Key is supported.
  Librem,
  /// Only the Nitrokey Pro is supported.
  Pro,
  /// Only the Nitrokey Storage is supported.
  Storage,
  /// Both the Nitrokey Pro and Storage are supported.
  Any,
}

/// A type used for "filtering" what device types to emit test code for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Filter {
  /// Only emit tests for a Librem Key.
  Librem,
  /// Only emit tests for a Nitrokey Pro.
  Pro,
  /// Only emit tests for a Nitrokey Storage.
  Storage,
}

impl Filter {
  pub fn from_attribute(attr: &TokenStream) -> Option<Self> {
    match attr.to_string().as_ref() {
      "librem" => Some(Filter::Librem),
      "pro" => Some(Filter::Pro),
      "storage" => Some(Filter::Storage),
      "" => None,
      _ => panic!("unexpected filter argument: {}", attr),
    }
  }
}


/// Apply a filter to a `SupportedDevice`.
///
/// Filtering basically produces the following outcomes:
/// #[test]          fn foo();                      -> no device
/// #[test(librem)]  fn foo();                      -> librem
/// #[test(pro)]     fn foo();                      -> pro
/// #[test(storage)] fn foo();                      -> storage
///
/// #[test]          fn foo(device: Librem);        -> librem
/// #[test(librem)]  fn foo(device: Librem);        -> librem
/// #[test(pro)]     fn foo(device: Librem);        -> error
/// #[test(storage)] fn foo(device: Librem);        -> error
///
/// #[test]          fn foo(device: Pro);           -> pro
/// #[test(librem)]  fn foo(device: Pro);           -> error
/// #[test(pro)]     fn foo(device: Pro);           -> pro
/// #[test(storage)] fn foo(device: Pro);           -> error
///
/// #[test]          fn foo(device: Storage);       -> storage
/// #[test(librem)]  fn foo(device: Storage);       -> error
/// #[test(pro)]     fn foo(device: Storage);       -> error
/// #[test(storage)] fn foo(device: Storage);       -> storage
///
/// #[test]          fn foo(device: DeviceWrapper); -> any
/// #[test(librem)]  fn foo(device: DeviceWrapper); -> librem
/// #[test(pro)]     fn foo(device: DeviceWrapper); -> pro
/// #[test(storage)] fn foo(device: DeviceWrapper); -> storage
///
/// #[test]          fn foo(model: Model);          -> any
/// #[test(librem)]  fn foo(model: Model);          -> librem
/// #[test(pro)]     fn foo(model: Model);          -> pro
/// #[test(storage)] fn foo(model: Model);          -> storage
fn filter_device(
  device: Option<SupportedDevice>,
  filter: Option<Filter>,
) -> Option<SupportedDevice>
{
  match device {
    None => match filter {
      None => None,
      // As can be seen from the table above, we have some logic in here
      // that is no longer strictly a filter, but rather an addition.
      // That is done mostly for the user's convenience.
      Some(Filter::Librem) => Some(SupportedDevice::Librem),
      Some(Filter::Pro) => Some(SupportedDevice::Pro),
      Some(Filter::Storage) => Some(SupportedDevice::Storage),
    },
    Some(SupportedDevice::Librem) => match filter {
      None |
      Some(Filter::Librem) => Some(SupportedDevice::Librem),
      Some(Filter::Pro) => panic!("unable to combine 'pro' filter with Librem device"),
      Some(Filter::Storage) => panic!("unable to combine 'storage' filter with Librem device"),
    },
    Some(SupportedDevice::Pro) => match filter {
      None |
      Some(Filter::Pro) => Some(SupportedDevice::Pro),
      Some(Filter::Librem) => panic!("unable to combine 'librem' filter with Pro device"),
      Some(Filter::Storage) => panic!("unable to combine 'storage' filter with Pro device"),
    },
    Some(SupportedDevice::Storage) => match filter {
      None |
      Some(Filter::Storage) => Some(SupportedDevice::Storage),
      Some(Filter::Librem) => panic!("unable to combine 'librem' filter with Storage device"),
      Some(Filter::Pro) => panic!("unable to combine 'pro' filter with Storage device"),
    },
    Some(SupportedDevice::Any) => match filter {
      None => Some(SupportedDevice::Any),
      Some(Filter::Librem) => Some(SupportedDevice::Librem),
      Some(Filter::Pro) => Some(SupportedDevice::Pro),
      Some(Filter::Storage) => Some(SupportedDevice::Storage),
    },
  }
}


/// The group a particular device belongs to.
#[derive(Clone, Copy, Debug)]
enum DeviceGroup {
  /// The group encompassing all tests that require no device to be
  /// present.
  No,
  /// The group containing all tests for the Librem Key.
  Librem,
  /// The group containing all tests for the Nitrokey Pro.
  Pro,
  /// The group containing all tests for the Nitrokey Storage.
  Storage,
}

impl AsRef<str> for DeviceGroup {
  fn as_ref(&self) -> &str {
    match *self {
      DeviceGroup::No => NITROKEY_GROUP_NODEV,
      DeviceGroup::Librem => NITROKEY_GROUP_LIBREM,
      DeviceGroup::Pro => NITROKEY_GROUP_PRO,
      DeviceGroup::Storage => NITROKEY_GROUP_STORAGE,
    }
  }
}

impl From<Option<SupportedDevice>> for DeviceGroup {
  fn from(device: Option<SupportedDevice>) -> Self {
    match device {
      None => DeviceGroup::No,
      Some(device) => match device {
        SupportedDevice::Librem => DeviceGroup::Librem,
        SupportedDevice::Pro => DeviceGroup::Pro,
        SupportedDevice::Storage => DeviceGroup::Storage,
        SupportedDevice::Any => panic!("an Any device cannot belong to a group"),
      }
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
/// Test functionality on any Nitrokey device, but leave the device
/// connection to the user and just provide the model:
/// ```rust,no_run
/// #[nitrokey_test::test]
/// fn some_other_nitrokey_test(model: nitrokey::Model) {
///   // Connect to a device of the provided model.
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
/// Test functionality on a Nitrokey Pro device, but leave the device
/// connection to the user:
/// ```rust,no_run
/// #[nitrokey_test::test(pro)]
/// fn some_other_pro_test() {
///   // Do something on a Pro device.
/// }
/// ```
///
/// A model can be provided optionally, like so:
/// ```rust,no_run
/// #[nitrokey_test::test(pro)]
/// fn some_other_pro_test(model: nitrokey::Model) {
///   assert_eq!(model, nitrokey::Model::Pro);
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
  let input = syn::parse_macro_input!(item as syn::ItemFn);
  let filter = Filter::from_attribute(&attr);
  let dev_type = determine_device(&input.sig.inputs);
  let (device, argument) = dev_type
    .map_or((None, None), |(device, argument)| {
      (Some(device), Some(argument))
    });
  let device = filter_device(device, filter);

  // Make clippy happy.
  drop(attr);

  match device {
    None => {
      let name = format!("{}", &input.sig.ident);
      expand_wrapper(name, None, argument, &input)
    },
    Some(SupportedDevice::Librem)
      | Some(SupportedDevice::Pro)
      | Some(SupportedDevice::Storage) => {
      let name = format!("{}", &input.sig.ident);
      expand_wrapper(name, device, argument, &input)
    },
    Some(SupportedDevice::Any) => {
      let name = format!("{}_librem", &input.sig.ident);
      let dev = Some(SupportedDevice::Librem);
      let librem = expand_wrapper(name, dev, argument, &input);

      let name = format!("{}_pro", &input.sig.ident);
      let dev = Some(SupportedDevice::Pro);
      let pro = expand_wrapper(name, dev, argument, &input);

      let name = format!("{}_storage", &input.sig.ident);
      let dev = Some(SupportedDevice::Storage);
      let storage = expand_wrapper(name, dev, argument, &input);

      // Emit a test for all supported devices.
      quote! {
        #librem
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
    DeviceGroup::Librem => quote! { manager.connect_librem() },
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
            #NITROKEY_GROUP_LIBREM |
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

fn expand_arg<P>(
  device: Option<SupportedDevice>,
  argument: Option<ArgumentType>,
  args: &punctuated::Punctuated<syn::FnArg, P>,
) -> Tokens
where
  P: quote::ToTokens,
{
  // Based on the device we want to emit a test function for we recreate
  // the expected full path of the type. That is necessary because
  // client code may have a "use" and may just contain a `Pro`, for
  // example, while we really need to work with the absolute path.
  let arg_type = match device {
    None => quote! {},
    Some(device) => match argument {
      None => quote! {},
      Some(ArgumentType::Device) => match device {
        SupportedDevice::Librem => quote! { ::nitrokey::Librem },
        SupportedDevice::Pro => quote! { ::nitrokey::Pro },
        SupportedDevice::Storage => quote! { ::nitrokey::Storage },
        SupportedDevice::Any => unreachable!(),
      },
      Some(ArgumentType::DeviceWrapper) => quote! { ::nitrokey::DeviceWrapper },
      Some(ArgumentType::Model) => quote! { ::nitrokey::Model },
    },
  };

  match args.first() {
    Some(arg) => match arg {
      syn::FnArg::Typed(pat_type) => {
        let arg = syn::FnArg::Typed(syn::PatType {
          attrs: Vec::new(),
          pat: pat_type.pat.clone(),
          colon_token: pat_type.colon_token,
          ty: Box::new(syn::Type::Path(syn::parse_quote! { #arg_type })),
        });
        quote! { #arg }
      }
      _ => panic!("unexpected test function argument"),
    },
    None => quote! {},
  }
}

fn expand_call(
  device: Option<SupportedDevice>,
  argument: Option<ArgumentType>,
  wrappee: &syn::ItemFn,
) -> Tokens
{
  let test_name = &wrappee.sig.ident;
  let group = DeviceGroup::from(device);
  let connect = expand_connect(group, &wrappee.sig.output);

  let call = match device {
    None => quote! { #test_name() },
    Some(device) => match argument {
      None => quote! { #test_name() },
      Some(ArgumentType::Device) => quote! { #test_name(device) },
      Some(ArgumentType::DeviceWrapper) => match device {
        SupportedDevice::Librem => {
          quote! {
            #test_name(::nitrokey::DeviceWrapper::Librem(device))
          }
        },
        SupportedDevice::Pro => {
          quote! {
            #test_name(::nitrokey::DeviceWrapper::Pro(device))
          }
        },
        SupportedDevice::Storage => {
          quote! {
            #test_name(::nitrokey::DeviceWrapper::Storage(device))
          }
        },
        SupportedDevice::Any => unreachable!(),
      },
      Some(ArgumentType::Model) => {
        let model = match device {
          SupportedDevice::Librem => quote! { ::nitrokey::Model::Librem },
          SupportedDevice::Pro => quote! { ::nitrokey::Model::Pro },
          SupportedDevice::Storage => quote! { ::nitrokey::Model::Storage },
          SupportedDevice::Any => unreachable!(),
        };
        quote! { #test_name(#model) }
      }
    },
  };

  match argument {
    None |
    Some(ArgumentType::Model) => {
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
    },
    Some(ArgumentType::Device) |
    Some(ArgumentType::DeviceWrapper) => {
      quote! {
        let mut manager = ::nitrokey::force_take().unwrap();
        let device = #connect;
        #call
      }
    }
  }
}

/// Emit code for a wrapper function around a Nitrokey test function.
fn expand_wrapper<S>(
  fn_name: S,
  device: Option<SupportedDevice>,
  argument: Option<ArgumentType>,
  wrappee: &syn::ItemFn,
) -> Tokens
where
  S: AsRef<str>,
{
  // Note that we need to rely on proc_macro2 here, because while the
  // compiler provided proc_macro has `Ident` and `Span` types, they
  // cannot be interpolated with quote!{} for lack of quote::ToTokens
  // implementations.
  let name = Ident::new(fn_name.as_ref(), Span::call_site());
  let attrs = &wrappee.attrs;
  let body = &wrappee.block;
  let test_name = &wrappee.sig.ident;
  let test_arg = expand_arg(device, argument, &wrappee.sig.inputs);
  let test_call = expand_call(device, argument, wrappee);

  let ret_type = match &wrappee.sig.output {
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

fn determine_device_for_arg(arg: &syn::FnArg) -> (SupportedDevice, ArgumentType) {
  match arg {
    syn::FnArg::Typed(pat_type) => {
      let type_ = &pat_type.ty;
      match &**type_ {
        syn::Type::Path(path) => {
          if path.path.segments.is_empty() {
            panic!("invalid function argument type: {}", quote! {#path});
          }

          let type_ = format!("{}", path.path.segments.last().unwrap().ident);
          match type_.as_ref() {
            "Model" => (SupportedDevice::Any, ArgumentType::Model),
            "Storage" => (SupportedDevice::Storage, ArgumentType::Device),
            "Pro" => (SupportedDevice::Pro, ArgumentType::Device),
            "Librem" => (SupportedDevice::Librem, ArgumentType::Device),
            "DeviceWrapper" => (SupportedDevice::Any, ArgumentType::DeviceWrapper),
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
fn determine_device<P>(
  args: &punctuated::Punctuated<syn::FnArg, P>,
) -> Option<(SupportedDevice, ArgumentType)>
where
  P: quote::ToTokens,
{
  match args.len() {
    0 => None,
    1 => Some(determine_device_for_arg(&args[0])),
    _ => panic!("functions used as Nitrokey tests can only have zero or one argument"),
  }
}


#[cfg(test)]
mod tests {
  use super::ArgumentType;
  use super::determine_device;
  use super::SupportedDevice;

  use syn;


  #[test]
  fn determine_nitrokey_none() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_none() {}
    };
    let dev_type = determine_device(&input.sig.inputs);

    assert_eq!(dev_type, None);
  }

  #[test]
  fn determine_librem() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_librem(device: nitrokey::Librem) {}
    };
    let dev_type = determine_device(&input.sig.inputs);

    assert_eq!(dev_type, Some((SupportedDevice::Librem, ArgumentType::Device)));
  }

  #[test]
  fn determine_nitrokey_pro() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_pro(device: nitrokey::Pro) {}
    };
    let dev_type = determine_device(&input.sig.inputs);

    assert_eq!(dev_type, Some((SupportedDevice::Pro, ArgumentType::Device)));
  }

  #[test]
  fn determine_nitrokey_storage() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_storage(device: nitrokey::Storage) {}
    };
    let dev_type = determine_device(&input.sig.inputs);

    assert_eq!(dev_type, Some((SupportedDevice::Storage, ArgumentType::Device)));
  }

  #[test]
  fn determine_any_nitrokey() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_any(device: nitrokey::DeviceWrapper) {}
    };
    let dev_type = determine_device(&input.sig.inputs);

    assert_eq!(dev_type, Some((SupportedDevice::Any, ArgumentType::DeviceWrapper)));
  }

  #[test]
  #[should_panic(expected = "functions used as Nitrokey tests can only have zero or one argument")]
  fn determine_wrong_argument_count() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_pro(device: nitrokey::Pro, _: i32) {}
    };
    let _ = determine_device(&input.sig.inputs);
  }

  #[test]
  #[should_panic(expected = "unexpected function argument signature: & self")]
  fn determine_wrong_function_type() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_self(&self) {}
    };
    let _ = determine_device(&input.sig.inputs);
  }

  #[test]
  #[should_panic(expected = "unexpected function argument type: & nitrokey \
                             :: DeviceWrapper (expected owned object)")]
  fn determine_wrong_argument_type() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_any(device: &nitrokey::DeviceWrapper) {}
    };
    let _ = determine_device(&input.sig.inputs);
  }

  #[test]
  #[should_panic(expected = "unsupported function argument type: FooBarBaz")]
  fn determine_invalid_argument_type() {
    let input: syn::ItemFn = syn::parse_quote! {
      #[nitrokey_test::test]
      fn test_foobarbaz(device: nitrokey::FooBarBaz) {}
    };
    let _ = determine_device(&input.sig.inputs);
  }
}
