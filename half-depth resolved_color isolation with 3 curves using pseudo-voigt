///////////////////////////////////////////////////////////////////////////////
// Summary of edits and current state
// - Purpose: depth-aware ColorIsolation shader derived from ColorIsolation2 with
//   extra depth, focus and debug features added during iterative development.
// - Features added in this branch/work session:
//   * Depth-aware masking with quantized controls for Depth Start / End / Falloff
//     (per-component quantization steps for fine resolution control).
//   * Depth buffer debug overlay (shows linearized depth and a curve overlay).
//   * Spatial depth smoothing with optional bilateral color weighting to preserve
//     edges when smoothing noisy depth.
//   * Integration of CinematicDOF CoC/focus math for focal exclusion masks:
//     ComputeFocalMask now uses CinematicDOF-style CoC calculations (focal length,
//     aperture, sensor size) and supports Auto-Focus sampling and a manual focus
//     distance in meters.
//   * Screen-center exclusion and hard-center exclude controls for preserving
//     subjects in full color regardless of depth sliders.
//   * UI organization: Depth / Setup / Debug categories; many sliders and toggles
//     added for interactive tuning (CoC falloff, FocalLength, FNumber, autofocus UV,
//     overlay controls, etc.).
// - Robustness and compatibility fixes applied across files:
//   * Replaced unsafe macros (BUFFER_WIDTH/HEIGHT) usages with `ReShade::ScreenSize`
//     or provided safe fallbacks where needed.
//   * Patched `CinematicDOF.fx` to avoid sampler-typed function parameters and
//     removed an unsafe `[unroll]` on a non-constant loop to prevent huge compile
//     unrolls (these changes addressed common x3511/x3570 build errors).
//   * Removed explicit texture metadata and avoided sampler parameters in helper
//     functions where toolchains reject them.
// - Seeded region-growing (experimental): implemented a ping-pong seed/grow
//   pipeline to better match subject silhouettes using depth + optional color
//   gating. This feature caused compatibility and RTV/SRV conflicts across some
//   toolchains and was later removed from this file to restore a safe, single-pass
//   shader state. The code history is still present in earlier edits but not active
//   in this current file.
// - Light-weight silhouette and middle-depth band experiments were added and
//   subsequently reverted; the current file contains the stable, single-pass
//   ColorIsolationPS that uses depth + focal/center exclude only.
// - Current status & recommended next steps:
//   * This file now compiles with the risky multi-pass seeded pipeline removed.
//   * If you want silhouette protection again, prefer a single-pass AutoFocusUV-
//     seeded approximation or a half-res ping-pong RT implementation to avoid
//     RTV/SRV sampling conflicts.
//   * If build errors persist in your environment, paste exact compiler output
//     (error text + line numbers) so I can iterate further.
//
// Notes: keep this header up-to-date with any future edits that reintroduce
// multi-pass features or extra render targets.
///////////////////////////////////////////////////////////////////////////////
//
//ReShade Shader: ColorIsolation2
//https://github.com/Daodan317081/reshade-shaders
//
//BSD 3-Clause License
//
//Copyright (c) 2018-2020, Alexander Federwisch
//All rights reserved.
//
//Redistribution and use in source and binary forms, with or without
//modification, are permitted provided that the following conditions are met:
//
//* Redistributions of source code must retain the above copyright notice, this
//  list of conditions and the following disclaimer.
//
//* Redistributions in binary form must reproduce the above copyright notice,
//  this list of conditions and the following disclaimer in the documentation
//  and/or other materials provided with the distribution.
//
//* Neither the name of the copyright holder nor the names of its
//  contributors may be used to endorse or promote products derived from
//  this software without specific prior written permission.
//
//THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
//AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
//FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
//SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
//CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
//OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///////////////////////////////////////////////////////////////////////////////

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

uniform float fDepthEnd <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth End";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.001;
    ui_tooltip = "Distance at which the effect is at its fullest.";
> = 0.1;

uniform float fDepthFalloff <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Falloff";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0;
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

// Quantize a normalized value `v` into `steps` discrete steps (returns float)
float Quantize(float v, int steps)
{
    int s = max(1, steps);
    return round(v * (float)s) / (float)s;
}

// Compute depth mask using quantized start/end/falloff values
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
    ui_min = 0.0; ui_max = 100.0; ui_step = 0.1;
> = 10.0;

uniform float fFocusRange <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Focus Range (meters)";
    ui_tooltip = "Half-width around the focus distance to consider in-focus (smooth falloff).";
    ui_min = 0.01; ui_max = 50.0; ui_step = 0.01;
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
    ui_min = 0.0; ui_max = 0.5; ui_step = 0.01;
> = 0.15;

uniform float fExcludeStrength <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Exclude Strength";
    ui_tooltip = "How strongly the focus/center mask overrides the depth mask (0 = none, 1 = full).";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.01;
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
    ui_min = 1.0; ui_max = 22.0; ui_step = 0.1;
> = 2.8;

// How quickly the CoC-based focal mask falls off with increasing CoC (in millimeters)
uniform float fCoCFalloffMM <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "CoC Falloff (mm)";
    ui_tooltip = "Controls the smooth falloff of the CoC-based focal mask in millimeters.";
    ui_min = 0.01; ui_max = 50.0; ui_step = 0.01;
> = 0.5;

// Sensor height used by CinematicDOF (35mm full-frame height = 24mm)
static const float COC_SENSOR_SIZE = 0.024;

// (Seeded region-growing removed)

// Auto-focus and hard-center exclusion
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
    ui_min = 0.0; ui_max = 0.5; ui_step = 0.001;
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

// --- Focus / Center exclusion helpers
// Returns 0..1 where 1 means fully "in-focus" (should be excluded from isolation)
// Compute focal mask using CinematicDOF's CoC math.
// linearDepth: normalized linear depth (0..1). focusDistMeters: focus distance in meters.
float ComputeFocalMask(float linearDepth, float focusDistMeters)
{
    if (!bExcludeFocus) return 0.0;

    // convert depths to meters
    float pixelDepthInM = linearDepth * 1000.0;
    float focusDepthInM = focusDistMeters;

    // Avoid division by zero when pixelDepthInM is zero
    float safePixelDepthInM = pixelDepthInM + (pixelDepthInM == 0.0 ? 1e-6 : 0.0);

    // CoC (cocInMM) calculation copied from CinematicDOF::CalculateBlurDiscSize (in mm)
    float cocInMM = (((FocalLength * FocalLength) / FNumber) / ((focusDepthInM / 1000.0) - FocalLength)) *
                     (abs(pixelDepthInM - focusDepthInM) / safePixelDepthInM);

    // Convert to normalized sensor fraction (0..1) similar to CinematicDOF's usage
    float cocNormalized = clamp(abs(cocInMM) * COC_SENSOR_SIZE, 0.0, 1.0);

    // Convert CoC magnitude into an in-focus mask: small CoC -> in focus (mask ~= 1), large CoC -> out of focus (mask ~= 0)
    // fCoCFalloffMM controls the smooth falloff in mm; compare against absolute CoC in mm
    float mask = saturate(1.0 - smoothstep(0.0, fCoCFalloffMM, abs(cocInMM)));

    return mask;
}

// texcoord is normalized (0..1)
float ComputeCenterMask(float2 texcoord)
{
    if (!bUseCenterMask || fCenterRadius <= 0.0) return 0.0;
    float centerDist = distance(texcoord, float2(0.5, 0.5));
    float m = saturate(1.0 - (centerDist / max(1e-5, fCenterRadius)));
    return m;
}

float ComputeExcludeMask(float2 texcoord, float linearDepth)
{
    // determine focus distance: manual or auto-sampled
    float focusDistMeters = fFocusDistance; // manual is expected in meters
    if (bUseAutoFocus)
    {
        // sample a small, stable depth at the autofocus UV (normalized), convert to meters
        float sampled = SmoothDepth(AutoFocusUV, max(0, iDepthSmoothRadius));
        focusDistMeters = sampled * 1000.0;
    }

    float focal = ComputeFocalMask(linearDepth, focusDistMeters);
    float center = ComputeCenterMask(texcoord);
    float mask = max(focal, center);

    // Hard exclude around the autofocus UV (use autofocus UV as the center for the hard mask)
    if (bHardCenterExclude)
    {
        float d = distance(texcoord, AutoFocusUV);
        if (d <= fHardExcludeRadius)
            mask = 1.0;
    }

    return saturate(mask * fExcludeStrength);
}

// (Silhouette protection removed)

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
    ui_min = 0.0; ui_max = 1.0;
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

uniform bool bDebugDepth <
    ui_type = "checkbox";
    ui_label = "Show Depth Buffer";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_tooltip = "Displays the linearized depth buffer in grayscale for debugging.";
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

uniform int iDepthSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Steps";
    ui_type = "slider";
    ui_min = 2; ui_max = 128;
    ui_tooltip = "Controls the precision of the depth mask transition. Higher values = smoother, more precise control.";
> = 32;

float3 RGBfromHue(float3 c) {
    const float3 A = float3(120.0, 60.0, 180.0)/360.0;
    const float3 B = float3(240.0, 180.0, 300.0)/360.0;
    float3 rgb = (saturate(-6.0 * (c.xxx - A)) + saturate(6.0 * (c.xxx - B)))*float3(1.0,-1.0,-1.0)+float3(0.0,1.0,1.0);
    return rgb;
}

float Map(float value, float2 span_old, float2 span_new) {
    float span_old_diff = abs(span_old.y - span_old.x) < 1e-6 ? 1e-6 : span_old.y - span_old.x;
    return lerp(span_new.x, span_new.y, (clamp(value, span_old.x, span_old.y)-span_old.x)/(span_old_diff));
}

//These RGB/HSV conversion functions are based on the blogpost from:
//http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
float3 RGBtoHSV(float3 c) {
    float4 K = float4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    float4 p = c.g < c.b ? float4(c.bg, K.wz) : float4(c.gb, K.xy);
    float4 q = c.r < p.x ? float4(p.xyw, c.r) : float4(c.r, p.yzx);

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return float3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

float Curve(float x, float p, float s, float h) {
    float offset = p.x;
    s = (1.1 - s) * 10.0;
    float sr = -smoothstep(0.0,1.0,s*(x-offset))+1;
    float sl = -smoothstep(0.0,1.0,s*((1-x)-(1-offset)))+1;
    float value = x < offset ? sl : sr;
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

float PreciseDepthMask(float depth, float start, float end, float falloff, int steps)
{
    float mask = smoothstep(start, end, depth) * (1.0 - smoothstep(end, end + falloff, depth));
    // Quantize the mask to the specified number of steps
    mask = round(mask * steps) / steps;
    return mask;
}

float3 DrawDebugOverlay(float3 background, float3 param, float2 pos, int2 size, float opacity, int2 vpos, float2 texcoord) {
    float x, y, value, value2, luma;
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
    // Get linearized depth (0.0 = camera, 1.0 = far plane)
    float depth = ReShade::GetLinearizedDepth(texcoord);
    // Optionally use a spatially-smoothed depth for masking
    float smooth_depth = SmoothDepth(texcoord, iDepthSmoothRadius);
    float3 color = tex2D(ReShade::BackBuffer, texcoord).rgb;
    float3 retVal;

    // Debug view for depth buffer
    if (bDebugDepth)
    {
        // Show depth buffer visualization
        float3 depth_vis = depth.xxx;
        retVal = depth_vis;
        
        // Apply the debug overlay on top of the depth visualization
        if(SHOW_DEBUG_OVERLAY)
        {
            retVal = DrawDebugOverlay(retVal, 1.0, DEBUG_OVERLAY_POSITION, DEBUG_OVERLAY_SIZE, DEBUG_OVERLAY_OPACITY, vpos.xy, texcoord);
        }
        return retVal;
    }

    float3 luma = dot(color, float3(0.2126, 0.7151, 0.0721)).rrr;
    
    // Calculate the original color isolation weight from the color's hue
    float value = CalculateWeight(RGBtoHSV(color).x, CURVE_CENTER, CURVE_OVERLAP, CURVE_HEIGHT);

    // Calculate a depth mask using the (optionally) smoothed depth
    float depth_factor = PreciseDepthMask(smooth_depth, fDepthStart, fDepthEnd, fDepthFalloff, iDepthStartSteps, iDepthEndSteps, iDepthFalloffSteps);

    // Compute an exclusion mask based on focal distance and/or screen-center
    float excludeMask = ComputeExcludeMask(texcoord, smooth_depth);

    // Combine masks (no silhouette/middle band)
    float combinedExclude = excludeMask;

    // When combinedExclude == 1, force the depth mask to 1.0 (i.e., keep full color regardless of sliders)
    float combinedDepthMask = lerp(depth_factor, 1.0, combinedExclude);

    // Modulate the isolation weight by the resulting combined depth mask
    float final_weight = lerp(1.0, value, combinedDepthMask);

    // Apply the final calculated color isolation
    retVal = lerp(luma, color, final_weight);

    if(SHOW_COLOR_DIFFERENCE)
    {
        // Show the final weight for debugging purposes.
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
        /* RenderTarget = BackBuffer */
    }
}
