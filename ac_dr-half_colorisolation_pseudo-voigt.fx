/*
 Color Isolation (Depth Logic Fix)
 - FIXED: Background objects (like the red railing) now correctly turn Grey.
 - FIXED: "Small Pixel" issue by prioritizing raw depth accuracy.
 - LOGIC: 
    1. Depth Mask defines the "Active Zone". 
    2. Everything OUTSIDE this zone is forced Grey.
    3. Everything INSIDE this zone follows the Color Curves.
    4. "Foreground Protection" overrides everything to keep the character in color.
*/

#include "ReShade.fxh"

#define COLORISOLATION_CATEGORY_SETUP "Setup"
#define COLORISOLATION_CATEGORY_DEPTH "Depth Masking (Background)"
#define COLORISOLATION_CATEGORY_EXCLUDE "Foreground Protection"
#define COLORISOLATION_CATEGORY_DEBUG "Debug"

// --- SETUP ---
uniform bool SHOW_DEBUG_OVERLAY <
    ui_label = "Show Curve Overlay";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = false;

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
    ui_tooltip = "Controls the width of the isolation bell curve.";
    ui_min = 0.0; ui_max = 1.0;
    ui_step = 0.001;
> = float3(0.5, 0.5, 0.5);

uniform bool ENABLE_CURVE2 <
    ui_type = "radio";
    ui_label = "Enable Curve 2";
    ui_category = COLORISOLATION_CATEGORY_SETUP;
> = false;

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

// --- DEPTH MASKING ---

uniform float fDepthStart <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Start";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.001;
> = 0.0;

uniform float fDepthGamma <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Reading Gamma";
    ui_type = "slider";
    ui_min = 0.001; ui_max = 5.0; ui_step = 0.001;
    ui_tooltip = "Controls the mid-tone distribution of the depth buffer.";
> = 1.0;

uniform float fDepthEnd <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth End";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.001;
> = 1.0; // Default to 1.0 so initially ALL depth is included (until user tweaks it)

uniform float fDepthFalloff <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Falloff";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.001;
> = 0.1;

uniform int iDepthStartSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Start Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
> = 1000;

uniform int iDepthEndSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth End Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
> = 1000;

uniform int iDepthFalloffSteps <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Falloff Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
> = 1000;

uniform bool bDebugDepth <
    ui_type = "checkbox";
    ui_label = "Show Depth Buffer";
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_tooltip = "Displays the linearized depth buffer in grayscale.";
> = false;

uniform bool bDepthSmooth <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Enable Depth Smoothing";
> = false; // Default OFF to preserve small pixels

uniform int iDepthSmoothRadius <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Depth Smooth Radius";
    ui_type = "slider";
    ui_min = 0; ui_max = 4;
> = 0;

uniform bool bDepthBilateral <
    ui_category = COLORISOLATION_CATEGORY_DEPTH;
    ui_label = "Bilateral Smoothing";
> = true;

// --- FOREGROUND PROTECTION (EXCLUDE) ---

uniform bool bExcludeFocus <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Exclude Focus Band";
    ui_tooltip = "Excludes pixels within the Focus Range of the Focus Depth from isolation.";
> = false;

uniform bool bExcludeForeground <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Exclude Foreground";
    ui_tooltip = "Protects everything closer than the focus depth (keeps it in color).";
> = true;

uniform bool bProtectSpecificColor <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Filter Protection by Color";
    ui_tooltip = "If enabled, foreground objects will ONLY be protected if they match the color below.";
> = false;

uniform float3 fProtectionColor <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_type = "color";
    ui_label = "Protection Target Color";
> = float3(1.0, 0.8, 0.6);

uniform float fProtectionColorRange <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_type = "slider";
    ui_label = "Protection Color Range";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.001;
> = 0.2;

uniform float fFocusDepth <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Focus Depth (0-1)";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.001;
> = 0.050;

uniform float fFocusRangeDepth <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Focus Band Width (0-1)";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.001;
> = 0.020;

uniform float fForegroundDepthOffset <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Protection Offset (0-1)";
    ui_type = "slider";
    ui_min = -0.1; ui_max = 0.1; ui_step = 0.0001;
> = 0.005;

uniform float fForegroundDepthFalloff <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Protection Falloff (0-1)";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 0.2; ui_step = 0.0001;
> = 0.010;

uniform int iForegroundSteps <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Protection Falloff Steps";
    ui_type = "slider";
    ui_min = 1; ui_max = 10000;
> = 1000;

uniform float fExcludeStrength <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Exclude Strength";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.01;
> = 1.0;

uniform bool bUseAutoFocus <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Use Auto-Focus Point";
> = true;

uniform float2 AutoFocusUV <
    ui_category = COLORISOLATION_CATEGORY_EXCLUDE;
    ui_label = "Auto-Focus UV";
    ui_type = "slider";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.01;
> = float2(0.5, 0.5);


// --- DEBUG UI ---

uniform bool SHOW_COLOR_DIFFERENCE <
    ui_type = "radio";
    ui_label = "Show Color Difference";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
> = false;

uniform bool SHOW_EXCLUDE_MASK <
    ui_type = "radio";
    ui_label = "Show Protection Mask";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_tooltip = "White = Protected (Full Color), Black = Isolated (Curves Apply)";
> = false;

uniform float2 DEBUG_OVERLAY_POSITION<
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_category_closed = true;
    ui_label = "Overlay Position";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.01;
> = float2(0.0, 0.15);

uniform int2 DEBUG_OVERLAY_SIZE <
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_category_closed = true;
    ui_label = "Overlay Size";
    ui_min = 50; ui_step = 1;
> = int2(1000, 300);

uniform float DEBUG_OVERLAY_OPACITY <
    ui_type = "drag";
    ui_category = COLORISOLATION_CATEGORY_DEBUG;
    ui_category_closed = true;
    ui_label = "Overlay Opacity";
    ui_min = 0.0; ui_max = 1.0; ui_step = 0.01;
> = 1.0;

// --- FUNCTIONS ---

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
    float sigma = max(0.001, s * 0.12); 
    float gaussian = exp(-0.5 * (dist * dist) / (sigma * sigma));
    float lorentzian = 1.0 / (1.0 + (dist * dist) / (sigma * sigma));
    float mix_ratio = 0.5;
    float value = lerp(gaussian, lorentzian, mix_ratio);
    return value * h;
}

float CalculateWeight(float x, float3 pos, float3 slope, float3 height) {
    float value = 0.0;
    value += Curve(x, pos.x, slope.x, height.x);
    value += Curve(x, pos.x + 1.0, slope.x, height.x);
    value += Curve(x, pos.x - 1.0, slope.x, height.x);

    if (ENABLE_CURVE2)
    {
        value += Curve(x, pos.y, slope.y, height.y);
        value += Curve(x, pos.y + 1.0, slope.y, height.y);
        value += Curve(x, pos.y - 1.0, slope.y, height.y);
    }

    if (ENABLE_CURVE3)
    {
        value += Curve(x, pos.z, slope.z, height.z);
        value += Curve(x, pos.z + 1.0, slope.z, height.z);
        value += Curve(x, pos.z - 1.0, slope.z, height.z);
    }

    value = saturate(value);
    return CURVE_INVERT ? 1.0 - value : value;
}

float SampleLinearDepth(float2 tc) {
    return ReShade::GetLinearizedDepth(tc);
}

float SmoothDepth(float2 tc, int radius) {
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
            float dist = length(float2(x, y));
            float w = exp(-dist * 0.8);

            if (bDepthBilateral) {
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

float Quantize(float v, int steps) {
    int s = max(1, steps);
    return round(v * (float)s) / (float)s;
}

float PreciseDepthMask(float depth, float start, float end, float falloff, int sSteps, int eSteps, int fSteps) {
    float sQ = Quantize(start, sSteps);
    float eQ = Quantize(end, eSteps);
    float fQ = Quantize(falloff, fSteps);
    if (eQ < sQ) eQ = sQ;
    float mask = smoothstep(sQ, eQ, depth) * (1.0 - smoothstep(eQ, eQ + fQ, depth));
    return saturate(mask);
}

// Updated Focal Mask
float ComputeFocalMask(float linearDepth, float focusDepth) {
    if (!bExcludeFocus) return 0.0;
    float dist = abs(linearDepth - focusDepth);
    float range = max(0.0001, fFocusRangeDepth);
    float val = dist / range;
    float mask = saturate(1.0 - val);
    return smoothstep(0.0, 1.0, mask);
}

// Updated Foreground Mask
float ComputeForegroundMask(float linearDepth, float focusDepth) {
    if (!bExcludeForeground) return 0.0;
    float protectionEnd = focusDepth + fForegroundDepthOffset;
    float falloff = Quantize(fForegroundDepthFalloff, iForegroundSteps);
    float mask = 1.0 - smoothstep(protectionEnd, protectionEnd + max(0.001, falloff), linearDepth);
    return saturate(mask);
}

// Color Protection
float ComputeColorProtection(float3 currColor) {
    if (!bProtectSpecificColor) return 1.0; 
    float diff = distance(currColor, fProtectionColor);
    return 1.0 - smoothstep(fProtectionColorRange * 0.5, fProtectionColorRange, diff);
}

// Updated Exclude Mask
float ComputeExcludeMask(float2 texcoord, float linearDepth, float3 currColor, float depthGamma) {
    float focusDepth = fFocusDepth;
    if (bUseAutoFocus) {
        float sampled = SmoothDepth(AutoFocusUV, max(0, iDepthSmoothRadius));
        focusDepth = sampled;
        focusDepth = pow(max(0.0, focusDepth), depthGamma);
    }

    float focal = ComputeFocalMask(linearDepth, focusDepth);
    float foreground = ComputeForegroundMask(linearDepth, focusDepth);
    
    float colorProtect = ComputeColorProtection(currColor);
    foreground *= colorProtect;

    float mask = max(focal, foreground);

    return saturate(mask * fExcludeStrength);
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
        
        float line_dist = abs((1.0 - y) - value);
        float line_alpha = exp(-size.y * 0.02 * line_dist);
        
        overlay = lerp(overlay, 0.0.rrr, line_alpha);
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
        float depth_vis = pow(max(0.0, smooth_depth), fDepthGamma);
        retVal = depth_vis.xxx;
        if(SHOW_DEBUG_OVERLAY)
        {
            retVal = DrawDebugOverlay(retVal, 1.0, DEBUG_OVERLAY_POSITION, DEBUG_OVERLAY_SIZE, DEBUG_OVERLAY_OPACITY, vpos.xy, texcoord);
        }
        return retVal;
    }

    float3 luma = dot(color, float3(0.2126, 0.7151, 0.0721)).rrr;
    float value = CalculateWeight(RGBtoHSV(color).x, CURVE_CENTER, CURVE_OVERLAP, CURVE_HEIGHT);

    // Depth logic
    float depth_for_isolation = pow(max(0.0, smooth_depth), fDepthGamma);
    float depth_factor = PreciseDepthMask(depth_for_isolation, fDepthStart, fDepthEnd, fDepthFalloff, iDepthStartSteps, iDepthEndSteps, iDepthFalloffSteps);
    
    // Exclude Logic (Protection)
    float excludeMask = ComputeExcludeMask(texcoord, depth_for_isolation, color, fDepthGamma);

    // LOGIC FIX:
    // depth_factor = 1.0 (Inside Zone) -> Use Curve Value.
    // depth_factor = 0.0 (Outside Zone) -> Force 0.0 (Grey).
    float isolation_weight = value * depth_factor;
    
    // Protection overrides everything to 1.0 (Color).
    float final_weight = max(isolation_weight, excludeMask);

    retVal = lerp(luma, color, final_weight);
    
    if(SHOW_EXCLUDE_MASK)
    {
        retVal = float3(excludeMask, excludeMask, excludeMask);
    }
    
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
