use crate::SerryInput;
use std::fs::File;
use std::net::TcpStream;

impl SerryInput for &[u8] {}
impl SerryInput for File {}
impl SerryInput for TcpStream {}
