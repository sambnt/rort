#version 450

layout(location = 0) in vec3 fragNormal;

layout(location = 0) out vec4 outColor;
layout(binding = 1) uniform sampler2D texSampler;

void main() {
    // outColor = texture(texSampler, fragTexCoord);
    outColor = vec4(fragNormal, 1.0);
}