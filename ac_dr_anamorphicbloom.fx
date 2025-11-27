/*
    Color-Isolated Soft Anamorphic Bloom (Double Width Version)
    Author: al_chan-Gemini Thinking 3.0 assisted
    Description: 
       - Multiplied horizontal spread factor by 2x for massive anamorphic streaks.
       - Integrated Color Isolation detection.
       - Smooth Gaussian blur with no blocky artifacts.
	   - High-intensity anamorphic streaks masked by depth. 
       - Includes Debug Mode to verify depth input.
       - Fixed X3511/X3570 errors using tex2Dlod.
	   - Generates intense horizontal streaks.
       - Occludes flares behind objects (Depth Masking).
       - FILTERS flares based on distance (Min Depth Check).
	   - Generates high-resolution, smooth OVAL-shaped streaks.
       - Solves blocky artifacts from low-res game lights.
       - Includes Depth Occlusion and Distance Filtering.
	   - Two-pass anamorphic bloom.
       - Includes Depth Occlusion and distance filtering.
       - NEW: Added forced smooth attenuation at the ends of the horizontal blur to prevent hard edges.
	   - Triggers flares based on COLOR MATCHING + Brightness.
       - Uses the Hybrid "Oval" shape with soft vertical blur.
       - Includes Depth Occlusion and Distance Filtering.
    License: CC0 (Public Domain)
	Credits :: J.J Abrams Lens Flare, Gemini Thinking 3.0
*/

#include "ReShade.fxh"

// --- Textures & Samplers ---
texture TexBlurH { Width = BUFFER_WIDTH / 2; Height = BUFFER_HEIGHT / 2; Format = RGBA16F; };
sampler SamplerBlurH { Texture = TexBlurH; };

// --- UI Parameters ---

uniform bool bShowDebug <
    ui_category = "Debug";
    ui_label = "Show Debug View";
    ui_tooltip = "Shows the intermediate bloom pass over the depth buffer for tuning color selection.";
> = false;

// --- COLOR DETECTION ---
uniform float3 cDetectColor <
    ui_category = "Color Detection";
    ui_label = "Target Detection Color";
    ui_tooltip = "The shader will look for this specific color to generate flares from.";
    ui_type = "color";
> = float3(0.0, 0.2, 1.0); 

uniform float fColorSensitivity <
    ui_category = "Color Detection";
    ui_label = "Color Sensitivity";
    ui_tooltip = "How strictly to match the color. Lower = stricter.";
    ui_type = "slider";
    ui_min = 0.01; ui_max = 1.0;
    ui_step = 0.01;
> = 0.25;

uniform float fBloomThreshold <
    ui_category = "Color Detection";
    ui_label = "Brightness Threshold";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.01;
> = 0.60;
// -----------------------

uniform float fBloomIntensity <
    ui_category = "Appearance";
    ui_label = "Bloom Intensity";
    ui_type = "drag";
    ui_min = 0.0; ui_max = 5.0;
    ui_step = 0.05;
> = 3.0;

uniform float3 cBloomColor <
    ui_category = "Appearance";
    ui_label = "Final Bloom Tint";
    ui_type = "color";
> = float3(0.0, 0.1, 1.0); 

uniform float fBlurWidthScale <
    ui_category = "Shape & Size";
    ui_label = "Blur Width (Stretch)";
    ui_tooltip = "Controls how wide the flare is. This version has 2x sensitivity.";
    ui_type = "slider";
    ui_min = 1.0; ui_max = 20.0;
    ui_step = 0.1;
> = 10.0;

uniform float fBlurHeightScale <
    ui_category = "Shape & Size";
    ui_label = "Blur Height (Softness)";
    ui_type = "slider";
    ui_min = 0.1; ui_max = 5.0;
    ui_step = 0.1;
> = 1.5;

uniform float fBloomGamma <
    ui_category = "Appearance";
    ui_label = "Intensity Curve";
    ui_type = "slider";
    ui_min = 1.0; ui_max = 5.0;
    ui_step = 0.1;
> = 2.5;

uniform float fFlareMinDepth <
    ui_category = "Distance / Occlusion";
    ui_label = "Min Flare Distance";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.001;
> = 0.0; 

uniform float fDepthOcclusionStrength <
    ui_category = "Distance / Occlusion";
    ui_label = "Occlusion Strength";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.01;
> = 0.98;

uniform float fDepthTolerance <
    ui_category = "Distance / Occlusion";
    ui_label = "Depth Edge Softness";
    ui_type = "slider";
    ui_min = 0.0001; ui_max = 0.02;
    ui_step = 0.0001;
> = 0.005;

uniform int iBlurSamples <
    ui_category = "Performance";
    ui_label = "Blur Samples";
    ui_tooltip = "If you see dots because of the extra width, increase this number.";
    ui_type = "slider";
    ui_min = 16; ui_max = 128;
    ui_step = 2;
> = 64;

// --- Shader Logic ---

float GetGaussianWeight(float offset, float sigma)
{
    return exp(-(offset * offset) / (2.0 * sigma * sigma));
}

// === PASS 1: HORIZONTAL BLUR (DOUBLED WIDTH) ===
float3 PS_HorizontalBlur(float4 vpos : SV_Position, float2 texcoord : TexCoord) : SV_Target
{
    float3 accumulatedColor = 0.0;
    float totalWeight = 0.0;
    float centerDepth = ReShade::GetLinearizedDepth(texcoord);
    float2 pixelSize = ReShade::PixelSize;

    float sigma = fBlurWidthScale;
    float maxOffset = float(iBlurSamples);

    [loop]
    for (int i = -iBlurSamples; i <= iBlurSamples; i++)
    {
        float offset = float(i);
        
        float gaussWeight = GetGaussianWeight(offset, sigma);
        float normalizedDist = abs(offset) / maxOffset;
        float edgeFade = pow(1.0 - normalizedDist, 2.0);
        float finalWeight = gaussWeight * edgeFade;

        if (finalWeight <= 0.0) continue;

        // --- CHANGE: Multiplied by 4.0 instead of 2.0 ---
        // This doubles the distance between samples, making the flare twice as long.
        float2 sampleUV = texcoord + float2(offset * pixelSize.x * 4.0, 0.0);
        
        if (sampleUV.x < 0.0 || sampleUV.x > 1.0) continue;
        
        // Distance Check
        float sourceDepth = ReShade::GetLinearizedDepth(sampleUV);
        if (sourceDepth < fFlareMinDepth) continue;

        // Sample Source Color
        float3 sourceColor = tex2Dlod(ReShade::BackBuffer, float4(sampleUV, 0.0, 0.0)).rgb;

        // Color Isolation
        float colorDiff = distance(normalize(sourceColor + 0.0001), normalize(cDetectColor + 0.0001));
        float colorMatch = 1.0 - saturate(colorDiff / fColorSensitivity);

        float luma = dot(sourceColor, float3(0.333, 0.333, 0.333));
        float brightnessFactor = max(0.0, luma - fBloomThreshold);

        float combinedStrength = brightnessFactor * colorMatch;

        if (combinedStrength <= 0.0001) continue;
        
        combinedStrength = pow(combinedStrength, fBloomGamma);

        // Occlusion
        float depthDiff = centerDepth - sourceDepth;
        float occlusionMask = smoothstep(-fDepthTolerance, fDepthTolerance*2.0, depthDiff); 
        occlusionMask = lerp(1.0, occlusionMask, fDepthOcclusionStrength);

        accumulatedColor += combinedStrength * finalWeight * occlusionMask;
        totalWeight += finalWeight;
    }

    return accumulatedColor / (totalWeight + 0.00001);
}

// === PASS 2: VERTICAL BLUR & COMBINE ===
float3 PS_VerticalCombine(float4 vpos : SV_Position, float2 texcoord : TexCoord) : SV_Target
{
    float3 originalColor = tex2D(ReShade::BackBuffer, texcoord).rgb;
    
    float3 accumulatedBloom = 0.0;
    float totalWeight = 0.0;
    float2 pixelSize = ReShade::PixelSize;

    float sigma = fBlurHeightScale;

    [loop]
    for (int i = -iBlurSamples/2; i <= iBlurSamples/2; i++) 
    {
        float offset = float(i);
        float weight = GetGaussianWeight(offset, sigma);

        float2 sampleUV = texcoord + float2(0.0, offset * pixelSize.y * 2.0);
        if (sampleUV.y < 0.0 || sampleUV.y > 1.0) continue;
        
        accumulatedBloom += tex2Dlod(SamplerBlurH, float4(sampleUV, 0.0, 0.0)).rgb * weight;
        totalWeight += weight;
    }

    float3 finalBloom = (accumulatedBloom / (totalWeight + 0.00001));
    finalBloom *= cBloomColor * fBloomIntensity;

    if (bShowDebug)
    {
        float centerDepth = ReShade::GetLinearizedDepth(texcoord);
        return float3(centerDepth, centerDepth, centerDepth) + finalBloom;
    }
    else
    {
        return originalColor + finalBloom;
    }
}

technique ColorIsolatedSoftBloom
{
    pass P0
    {
        VertexShader = PostProcessVS;
        PixelShader = PS_HorizontalBlur;
        RenderTarget = TexBlurH;
    }
    pass P1
    {
        VertexShader = PostProcessVS;
        PixelShader = PS_VerticalCombine;
    }

}
