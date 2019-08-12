// mod.rs

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

use nitrokey::CommunicationError;
use nitrokey::Device;
use nitrokey::Error;
use nitrokey::Model;


#[nitrokey_test::test]
fn no_dev() {
  let mut manager = nitrokey::force_take().unwrap();
  let error = manager.connect().unwrap_err();
  match error {
    Error::CommunicationError(CommunicationError::NotConnected) => (),
    _ => panic!("received unexpected error: {:?}", error),
  }
}

#[nitrokey_test::test]
fn pro(device: Pro) {
  assert_eq!(device.get_model(), Model::Pro);

  let manager = device.into_manager();
  assert!(manager.connect_pro().is_ok())
}

#[nitrokey_test::test(pro)]
fn pro_filter() {
  let mut manager = nitrokey::force_take().unwrap();
  assert!(manager.connect_pro().is_ok());
}

#[nitrokey_test::test]
fn storage(device: Storage) {
  assert_eq!(device.get_model(), Model::Storage);

  let manager = device.into_manager();
  assert!(manager.connect_storage().is_ok())
}

#[nitrokey_test::test(storage)]
fn storage_filter() {
  let mut manager = nitrokey::force_take().unwrap();
  assert!(manager.connect_storage().is_ok());
}

#[nitrokey_test::test]
fn any(device: DeviceWrapper) {
  let model = device.get_model();

  let manager = device.into_manager();
  assert!(manager.connect_model(model).is_ok())
}

#[nitrokey_test::test(storage)]
fn any_storage_filter(device: DeviceWrapper) {
  match device {
    nitrokey::DeviceWrapper::Storage(_) => (),
    _ => panic!("received invalid model: {:?}", device),
  }
}

#[nitrokey_test::test(pro)]
fn any_pro_filter(device: DeviceWrapper) {
  match device {
    nitrokey::DeviceWrapper::Pro(_) => (),
    _ => panic!("received invalid model: {:?}", device),
  }
}

#[nitrokey_test::test]
#[ignore]
fn ignore_no_dev() {
  panic!("should be ignored")
}

#[nitrokey_test::test]
#[ignore]
fn ignore_any(_device: nitrokey::DeviceWrapper) {
  panic!("should be ignored")
}


/// A trait providing a method with a &mut self signature.
trait MutableDevice<'mgr> {
  fn test_mut(&mut self) -> bool {
    true
  }
}

impl<'mgr> MutableDevice<'mgr> for nitrokey::DeviceWrapper<'mgr> {}

#[nitrokey_test::test]
fn mutable_device(mut device: nitrokey::DeviceWrapper) {
  assert!(device.test_mut())
}

#[nitrokey_test::test]
fn aribtrary_argument_name(foobarbaz: nitrokey::DeviceWrapper) {
  let _ = foobarbaz.get_model();
}
