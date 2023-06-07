use std::fs::File;
use std::net::TcpStream;
use crate::SerryInput;

impl SerryInput for &[u8] {}
impl SerryInput for File {}
impl SerryInput for TcpStream {}