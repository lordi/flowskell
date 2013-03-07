float3 Hue(float H)
{
    float R = abs(H * 6 - 3) - 1;
    float G = 2 - abs(H * 6 - 2);
    float B = 2 - abs(H * 6 - 4);
    return saturate(float3(R,G,B));
}

float3 HSVtoRGB(in float3 HSV)
{
    return ((Hue(HSV.x) - 1) * HSV.y + 1) * HSV.z;
}

This is particularly efficient because 'abs' and 'saturate' are "free" operations on a lot of GPU pipelines.

The reverse conversion is a bit more tricky, and if you're really obsessed with performance on Xbox360, or the like, you'll need to drop down into shader assembler to utilise the vector 'max4' instruction instead of the scalar alternatives:

float3 RGBtoHSV(in float3 RGB)
{
    float3 HSV = 0;
#if NO_ASM
    HSV.z = max(RGB.r, max(RGB.g, RGB.b));
    float M = min(RGB.r, min(RGB.g, RGB.b));
    float C = HSV.z - M;
#else
    float4 RGBM = RGB.rgbr;
    asm { max4 HSV.z, RGBM };
    asm { max4 RGBM.w, -RGBM };
    float C = HSV.z + RGBM.w;
#endif
    if (C != 0)
    {
        HSV.y = C / HSV.z;
        float3 Delta = (HSV.z - RGB) / C;
        Delta.rgb -= Delta.brg;
        Delta.rg += float2(2,4);
        if (RGB.r >= HSV.z)
            HSV.x = Delta.b;
        else if (RGB.g >= HSV.z)
            HSV.x = Delta.r;
        else
            HSV.x = Delta.g;
        HSV.x = frac(HSV.x / 6);
    }
    return HSV;
}
