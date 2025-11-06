pub fn clear_escape_codes(buffer: &[u8]) -> Vec<u8>
{
	let mut new_buffer: Vec<u8> = Vec::new();
	let mut in_escape_sequence: bool = false;
	buffer.iter().for_each(|byte: &u8| {
		let byte: u8 = *byte;
		if in_escape_sequence
		{
			if in_escape_sequence && byte == /* 'm' */ 109
			{
				in_escape_sequence = false;
			}
		}
		else if byte == /* ESC */ 27
		{
			in_escape_sequence = true;
		}
		else
		{
			new_buffer.push(byte);
		}
	});
	new_buffer
}

pub fn clear_escape_codes_from_str(text: &str) -> String
{
	let buffer: Vec<u8> = clear_escape_codes(text.as_bytes());
	String::from_utf8(buffer).unwrap()
}
