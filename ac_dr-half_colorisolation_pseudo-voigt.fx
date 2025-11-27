/*
 Summary of edits and current state
 - Purpose: depth-aware ColorIsolation shader with Pseudo-Voigt isolation.
 - UPDATES: 
   * ADDED 'fDepthGamma' slider between Depth Start and Depth End.
     This allows non-linear control of the depth reading (Midtone bias)
     to better isolate ranges that are "clumped" in the linear depth buffer.
   * 'bDebugDepth' is clustered in the Depth category.
   * Uses Pseudo-Voigt distribution for color isolation curves.

ReShade Shader: ColorIsolation2
https://github.com/Daodan317081/reshade-shaders

BSD 3-Clause License

Copyright (c) 2018-2020, Alexander Federwisch
All rights reserved.
*/

#include "ReShade.fxh"

#define COLORISOLATION_CATEGORY_SETUP "Setup"
#define COLORISOLATION_CATEGORY_DEPTH "Depth"
#define COLORISOLATION_CATEGORY_DEBUG "Debug"

uniform float fDepthStart <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Start";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.001;
    ui_tooltip = "Distance at which the effect starts to appear (0.0 = camera).";
> = 0.0;

// NEW: Gamma slider to control the curve "between" Start and End
uniform float fDepthGamma <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Reading Gamma";
    ui_type = "slider";
    ui_min = 0.001; ui_max = 5.0;
    ui_step = 0.001;
    ui_tooltip = "Controls the mid-tone distribution of the depth buffer.\n1.0 = Linear.\nLower = Expands near depth.\nHigher = Expands far depth.";
> = 1.0;

uniform float fDepthEnd <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth End";
    ui_type = "slider";
    ui_min = 0.0;
    ui_max = 1.0;
    ui_step = 0.001;
    ui_tooltip = "Distance at which the effect is at its fullest.";
> = 0.1;

uniform float fDepthFalloff <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Falloff";
    ui_type = "slider";
    ui_min = 0.0;
    ui_max = 1.0;
    ui_step = 0.001;
    ui_tooltip = "Distance over which the effect fades out after 'Depth End'.";
> = 0.1;

// --- Depth quantization steps (controls "step resolution" for start/end/falloff)
uniform int iDepthStartSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Start Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
    ui_tooltip = "Number of quantization steps for Depth Start (higher = finer control).";
> = 1000;

uniform int iDepthEndSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth End Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
    ui_tooltip = "Number of quantization steps for Depth End (higher = finer control).";
> = 1000;

uniform int iDepthFalloffSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Falloff Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
    ui_tooltip = "Number of quantization steps for Depth Falloff (higher = finer control).";
> = 1000;

// MOVED: Debug Depth is now clustered here with the depth step controls
uniform bool bDebugDepth <
    ui_type = "checkbox";
    ui_label = "Show Depth Buffer";
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_tooltip = "Displays the linearized depth buffer in grayscale for debugging.";
> = false;

// Quantize a normalized value `v` into `steps` discrete steps (returns float)
float Quantize(float v, int steps)
{
    int s = max(1, steps);
    return round(v * (float)s) / (float)s;
}

// Fixed signature accepting separate steps
float PreciseDepthMask(float depth, float start, float end, float falloff, int sSteps, int eSteps, int fSteps)
{
    float sQ = Quantize(start, sSteps);
    float eQ = Quantize(end, eSteps);
    float fQ = Quantize(falloff, fSteps);
    // Safety: ensure end is not below start after quantization
    if (eQ < sQ) eQ = sQ;
    float mask = smoothstep(sQ, eQ, depth) * (1.0 - smoothstep(eQ, eQ + fQ, depth));
    return saturate(mask);
}

// --- Depth smoothing (spatial / optional bilateral) ---
uniform bool bDepthSmooth <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Enable Depth Smoothing";
    ui_tooltip = "Enable spatial smoothing of the linearized depth before masking.";
> = true;

uniform int iDepthSmoothRadius <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Smooth Radius";
    ui_type = "slider";
    ui_min = 0; ui_max = 4;
    ui_tooltip = "Radius (in pixels) used for spatial depth smoothing. 0 = off.";
> = 1;

uniform bool bDepthBilateral <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Bilateral Smoothing";
    ui_tooltip = "When enabled, smoothing weights are modulated by color similarity to preserve edges.";
> = true;

// --- Focus/Center exclusion (keep subject in full color regardless of depth sliders)
uniform bool bExcludeFocus <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Exclude Focus";
    ui_tooltip = "When enabled, pixels near the focus distance or near screen center will be excluded from color isolation.";
> = true;

uniform float fFocusDistance <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Focus Distance (meters)";
    ui_tooltip = "Linearized world-space distance considered 'in focus' (near this distance will be excluded).";
    ui_min = 0.0; ui_max = 100.0;
    ui_step = 0.1;
> = 10.0;

uniform float fFocusRange <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Focus Range (meters)";
    ui_tooltip = "Half-width around the focus distance to consider in-focus (smooth falloff).";
    ui_min = 0.01;
    ui_max = 50.0; ui_step = 0.01;
> = 1.5;

uniform bool bUseCenterMask <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Use Screen-Center Mask";
    ui_tooltip = "Also exclude pixels near the screen center (useful to keep the character).";
> = true;

uniform float fCenterRadius <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Center Radius";
    ui_tooltip = "Radius (0..0.5) of the screen-center exclusion mask; 0 = disabled.";
    ui_min = 0.0; ui_max = 0.5;
    ui_step = 0.01;
> = 0.15;

uniform float fExcludeStrength <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Exclude Strength";
    ui_tooltip = "How strongly the focus/center mask overrides the depth mask (0 = none, 1 = full).";
    ui_min = 0.0;
    ui_max = 1.0; ui_step = 0.01;
> = 1.0;

// Cinematic DoF lens parameters (used for CoC calculation)
uniform float FocalLength <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Focal Length (mm)";
    ui_min = 10; ui_max = 300; ui_step = 1;
> = 100.0;

uniform float FNumber <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Aperture (f-number)";
    ui_min = 1.0; ui_max = 22.0;
    ui_step = 0.1;
> = 2.8;

uniform float fCoCFalloffMM <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "CoC Falloff (mm)";
    ui_tooltip = "Controls the smooth falloff of the CoC-based focal mask in millimeters.";
    ui_min = 0.01; ui_max = 50.0; ui_step = 0.01;
> = 0.5;

static const float COC_SENSOR_SIZE = 0.024;

uniform bool bUseAutoFocus <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Use Auto-Focus Point";
    ui_tooltip = "Sample depth at the Auto-Focus UV to set the focus distance automatically.";
> = true;

uniform float2 AutoFocusUV <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Auto-Focus UV";
    ui_tooltip = "Normalized screen UV used for auto-focus sampling (0..1).";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.01;
> = float2(0.5, 0.5);

uniform bool bHardCenterExclude <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Hard Center Exclude";
    ui_tooltip = "Always exclude pixels within the Hard Exclude radius around the Auto-Focus UV regardless of depth.";
> = false;

uniform float fHardExcludeRadius <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Hard Exclude Radius";
    ui_tooltip = "Normalized radius (0..0.5) around Auto-Focus UV to hard-exclude from isolation.";
    ui_min = 0.0; ui_max = 0.5;
    ui_step = 0.001;
> = 0.001;

float SampleLinearDepth(float2 tc)
{
    return ReShade::GetLinearizedDepth(tc);
}

float SmoothDepth(float2 tc, int radius)
{
    if (radius <= 0 || !bDepthSmooth) return SampleLinearDepth(tc);
    float2 px = 1.0 / ReShade::ScreenSize;
    float sum = 0.0;
    float wsum = 0.0;
    float centerDepth = SampleLinearDepth(tc);
    float3 centerColor = tex2D(ReShade::BackBuffer, tc).rgb;

    for (int y = -radius; y <= radius; ++y)
    {
        for (int x = -radius; x <= radius; ++x)
        {
            float2 off = float2(x, y) * px;
            float d = SampleLinearDepth(tc + off);

            // spatial weight (simple gaussian-ish by distance)
            float dist = length(float2(x, y));
            float w = exp(-dist * 0.8);

            // bilateral color weight to preserve edges
            if (bDepthBilateral)
            {
                float3 c = tex2D(ReShade::BackBuffer, tc + off).rgb;
                float cd = length(c - centerColor);
                w *= exp(-cd * 10.0);
            }

            sum += d * w;
            wsum += w;
        }
    }

    return wsum > 0.0 ? sum / wsum : centerDepth;
}

float ComputeFocalMask(float linearDepth, float focusDistMeters)
{
    if (!bExcludeFocus) return 0.0;
    float pixelDepthInM = linearDepth * 1000.0;
    float focusDepthInM = focusDistMeters;
    float safePixelDepthInM = pixelDepthInM + (pixelDepthInM == 0.0 ? 1e-6 : 0.0);
    float cocInMM = (((FocalLength * FocalLength) / FNumber) / ((focusDepthInM / 1000.0) - FocalLength)) *
                     (abs(pixelDepthInM - focusDepthInM) / safePixelDepthInM);
    float cocNormalized = clamp(abs(cocInMM) * COC_SENSOR_SIZE, 0.0, 1.0);
    float mask = saturate(1.0 - smoothstep(0.0, fCoCFalloffMM, abs(cocInMM)));
    return mask;
}

float ComputeCenterMask(float2 texcoord)
{
    if (!bUseCenterMask || fCenterRadius <= 0.0) return 0.0;
    float centerDist = distance(texcoord, float2(0.5, 0.5));
    float m = saturate(1.0 - (centerDist / max(1e-5, fCenterRadius)));
    return m;
}

float ComputeExcludeMask(float2 texcoord, float linearDepth)
{
    float focusDistMeters = fFocusDistance;
    if (bUseAutoFocus)
    {
        float sampled = SmoothDepth(AutoFocusUV, max(0, iDepthSmoothRadius));
        focusDistMeters = sampled * 1000.0;
    }

    float focal = ComputeFocalMask(linearDepth, focusDistMeters);
    float center = ComputeCenterMask(texcoord);
    float mask = max(focal, center);

    if (bHardCenterExclude)
    {
        float d = distance(texcoord, AutoFocusUV);
        if (d <= fHardExcludeRadius)
            mask = 1.0;
    }

    return saturate(mask * fExcludeStrength);
}

uniform bool SHOW_DEBUG_OVERLAY <
    ui_label = "Show Overlay";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = false;

uniform bool ENABLE_CURVE2 <
    ui_type = "radio";
    ui_label = "Enable Curve 2";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = false;

uniform bool BOOL_UNUSED <
    ui_type = "radio";
    ui_label = "Left Values: Curve 1, Right Values: Curve 2";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = true;

uniform float3 CURVE_CENTER <
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
    ui_label = "Hue (Curve 1,2,3)";
    ui_tooltip = "Select the hue centers to isolate";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.005;
> = float3(0.0, 0.33, 0.66);

uniform float3 CURVE_HEIGHT<
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
    ui_label = "Strength (Curve 1,2,3)";
    ui_tooltip = "Select the saturation of the isolated color for each curve";
    ui_min = 0.0;
    ui_max = 1.0;
    ui_step = 0.005;
> = float3(1.0, 1.0, 1.0);

uniform float3 CURVE_OVERLAP <
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
    ui_label = "Overlap (Curve 1,2,3)";
    ui_tooltip = "Select how much neighbouring colors to isolate for each curve";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.001;
> = float3(0.5, 0.5, 0.5);

uniform bool ENABLE_CURVE3 <
    ui_type = "radio";
    ui_label = "Enable Curve 3";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = false;

uniform bool CURVE_INVERT <
    ui_type = "radio";
    ui_label = "Invert";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = false;

uniform bool SHOW_COLOR_DIFFERENCE <
    ui_type = "radio";
    ui_label = "Show Color Difference";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
> = false;

uniform float2 DEBUG_OVERLAY_POSITION<
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_category_closed = true;
    ui_label = "Overlay Position";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.01;
> = float2(0.0, 0.15);

uniform int2 DEBUG_OVERLAY_SIZE <
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_category_closed = true;
    ui_label = "Overlay Size";
    ui_tooltip = "x: width\nz: height";
    ui_min = 50;
    ui_step = 1;
> = int2(1000, 300);

uniform float DEBUG_OVERLAY_OPACITY <
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_category_closed = true;
    ui_label = "Overlay Opacity";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.01;
> = 1.0;

float3 RGBfromHue(float3 c) {
    const float3 A = float3(120.0, 60.0, 180.0)/360.0;
    const float3 B = float3(240.0, 180.0, 300.0)/360.0;
    float3 rgb = (saturate(-6.0 * (c.xxx - A)) + saturate(6.0 * (c.xxx - B)))*float3(1.0,-1.0,-1.0)+float3(0.0,1.0,1.0);
    return rgb;
}

float Map(float value, float2 span_old, float2 span_new) {
    float span_old_diff = abs(span_old.y - span_old.x) < 1e-6 ?
    1e-6 : span_old.y - span_old.x;
    return lerp(span_new.x, span_new.y, (clamp(value, span_old.x, span_old.y)-span_old.x)/(span_old_diff));
}

float3 RGBtoHSV(float3 c) {
    float4 K = float4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    float4 p = c.g < c.b ? float4(c.bg, K.wz) : float4(c.gb, K.xy);
    float4 q = c.r < p.x ?
    float4(p.xyw, c.r) : float4(c.r, p.yzx);

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return float3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

// Pseudo-Voigt Curve
float Curve(float x, float p, float s, float h) {
    float dist = x - p;
    float sigma = max(0.001, s * 0.5); 
    float gaussian = exp(-0.5 * (dist * dist) / (sigma * sigma));
    float lorentzian = 1.0 / (1.0 + (dist * dist) / (sigma * sigma));
    float mix_ratio = 0.5; 
    float value = lerp(gaussian, lorentzian, mix_ratio);
    return value * h;
}

float CalculateWeight(float x, float3 pos, float3 slope, float3 height) {
    float value = 0.0;
    // curve 1
    value += Curve(x, pos.x, slope.x, height.x);
    value += Curve(x, pos.x + 1.0, slope.x, height.x);
    value += Curve(x, pos.x - 1.0, slope.x, height.x);

    // curve 2 (optional)
    if (ENABLE_CURVE2)
    {
        value += Curve(x, pos.y, slope.y, height.y);
        value += Curve(x, pos.y + 1.0, slope.y, height.y);
        value += Curve(x, pos.y - 1.0, slope.y, height.y);
    }

    // curve 3 (optional)
    if (ENABLE_CURVE3)
    {
        value += Curve(x, pos.z, slope.z, height.z);
        value += Curve(x, pos.z + 1.0, slope.z, height.z);
        value += Curve(x, pos.z - 1.0, slope.z, height.z);
    }

    value = saturate(value);
    return CURVE_INVERT ? 1.0 - value : value;
}

float3 DrawDebugOverlay(float3 background, float3 param, float2 pos, int2 size, float opacity, int2 vpos, float2 texcoord) {
    float x, y, value, luma;
    float3 overlay, hsvStrip;

	float2 overlayPos = pos * (ReShade::ScreenSize - size);
    if(all(vpos.xy >= overlayPos) && all(vpos.xy < overlayPos + size))
    {
        x = Map(texcoord.x, float2(overlayPos.x, overlayPos.x + size.x) / ReShade::ScreenSize.x, float2(0.0, 1.0));
        y = Map(texcoord.y, float2(overlayPos.y, overlayPos.y + size.y) / ReShade::ScreenSize.y, float2(0.0, 1.0));
        hsvStrip = RGBfromHue(float3(x, 1.0, 1.0));
        luma = dot(hsvStrip, float3(0.2126, 0.7151, 0.0721));
        value = CalculateWeight(x, CURVE_CENTER, CURVE_OVERLAP, CURVE_HEIGHT);
        overlay = lerp(luma.rrr, hsvStrip, value);
        overlay = lerp(overlay, 0.0.rrr, exp(-size.y * length(float2(x, 1.0 - y) - float2(x, value))));
        background = lerp(background, overlay, opacity);
    }

    return background;
}

float3 ColorIsolationPS(float4 vpos : SV_Position, float2 texcoord : TexCoord) : SV_Target {
    float depth = ReShade::GetLinearizedDepth(texcoord);
    float smooth_depth = SmoothDepth(texcoord, iDepthSmoothRadius);
    float3 color = tex2D(ReShade::BackBuffer, texcoord).rgb;
    float3 retVal;

    if (bDebugDepth)
    {
        float3 depth_vis = depth.xxx;
        // Optionally visualize the Gamma correction in the debug view so you can see the curve
        // float3 depth_vis = pow(depth, fDepthGamma).xxx; 
        retVal = depth_vis;
        if(SHOW_DEBUG_OVERLAY)
        {
            retVal = DrawDebugOverlay(retVal, 1.0, DEBUG_OVERLAY_POSITION, DEBUG_OVERLAY_SIZE, DEBUG_OVERLAY_OPACITY, vpos.xy, texcoord);
        }
        return retVal;
    }

    float3 luma = dot(color, float3(0.2126, 0.7151, 0.0721)).rrr;
    float value = CalculateWeight(RGBtoHSV(color).x, CURVE_CENTER, CURVE_OVERLAP, CURVE_HEIGHT);
    
    // Apply Gamma Correction to the depth used for Isolation Mask
    // This allows better "Reading" of the depth buffer (expanding mids).
    // Note: We use max(0.0, ...) to ensure no negative numbers (safety).
    float depth_for_isolation = pow(max(0.0, smooth_depth), fDepthGamma);

    // Use the gamma-corrected depth for the mask calculation
    float depth_factor = PreciseDepthMask(depth_for_isolation, fDepthStart, fDepthEnd, fDepthFalloff, iDepthStartSteps, iDepthEndSteps, iDepthFalloffSteps);
    
    // Use the physical linear depth for Focus/Exclude (to keep meter units accurate)
    float excludeMask = ComputeExcludeMask(texcoord, smooth_depth);
    
    float combinedExclude = excludeMask;
    float combinedDepthMask = lerp(depth_factor, 1.0, combinedExclude);
    float final_weight = lerp(1.0, value, combinedDepthMask);
    retVal = lerp(luma, color, final_weight);
    
    if(SHOW_COLOR_DIFFERENCE)
    {
        retVal = final_weight.rrr;
    }
    
    if(SHOW_DEBUG_OVERLAY)
    {
        retVal = DrawDebugOverlay(retVal, 1.0, DEBUG_OVERLAY_POSITION, DEBUG_OVERLAY_SIZE, DEBUG_OVERLAY_OPACITY, vpos.xy, texcoord);
    }

    return retVal;
}

technique ColorIsolation2 {
    pass Final {
        VertexShader = PostProcessVS;
        PixelShader = ColorIsolationPS;
    }
}
