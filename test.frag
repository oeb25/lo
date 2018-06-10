struct VSOut {
	vec2 VSOut;
};
uniform sampler2D tex;
uniform float scale;

vec2 vec2() {

}
vec3 frag(vec2 TexCoords) {
	vec2 tex_offset = 1.0 / textureSize(tex, 0);
	vec3 c = sampleTex(TexCoords);
	bool horizontal = mod(scale, 2) == 0;
	for (int i = 0; i < 5; ++i) {
		vec2 move_by;
		if (horizontal) {	
			move_by = vec2(tex_offset.x * i, 0);
		} else {	
			move_by = vec2(0, tex_offset.y * i);
		};
		c += sampleTex(TexCoords + move_by);
		c += sampleTex(TexCoords - move_by);
	};
	return c;
}
VSOut vert() {
	VSOut out;
	return out;
}
vec3 vec3() {

}
float mod() {

}
float dot() {

}
vec3 sampleTex(vec2 coords) {
	vec3 color = vec3(texture(tex, coords));
	if (scale != 2.0) {	
		return color;
	} else {
		float brightness = dot(color, vec3(0.2126, 0.7152, 0.0722));
		if (brightness > 0.04) {	
			return color;
		} else {
			
			return vec3(0);
		};
	};
}
vec2 textureSize() {

}

