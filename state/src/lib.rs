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

#![warn(
  future_incompatible,
  missing_copy_implementations,
  missing_debug_implementations,
  rust_2018_compatibility,
  rust_2018_idioms,
  trivial_casts,
  trivial_numeric_casts,
  unstable_features,
  unused_import_braces,
  unused_qualifications,
  unused_results,
)]

//! A crate capturing the state needed by the `nitrokey-test` crate.

use std::sync;

/// A function returning a `Mutex` used for serializing tests.
pub fn mutex() -> &'static sync::Mutex<()> {
  static mut MUTEX: Option<sync::Mutex<()>> = None;
  static ONCE: sync::Once = sync::Once::new();

  ONCE.call_once(|| unsafe { MUTEX = Some(sync::Mutex::new(())) });

  match unsafe { &MUTEX } {
    Some(mutex) => mutex,
    None => unreachable!(),
  }
}
