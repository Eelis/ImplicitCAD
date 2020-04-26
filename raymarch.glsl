// The MIT License
// Copyright © 2013 Inigo Quilez
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions: The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#if HW_PERFORMANCE==0
#define AA 1
#else
#define AA 2   // make this 2 or 3 for antialiasing
#endif

//------------------------------------------------------------------

vec3 rotateX( vec3 p, float a )
{
    return vec3(p.x,
            cos(-a) * p.y - sin(-a) * p.z,
            sin(-a) * p.y + cos(-a) * p.z);
}

vec3 rotateY( vec3 p, float a )
{
    return vec3(
        cos(a) * p.x - sin(a) * p.z,
        p.y,
        sin(a) * p.x + cos(a) * p.z);
}

vec3 rotateZ( vec3 p, float a )
{
    return vec3(
        cos(-a) * p.x - sin(-a) * p.y,
        sin(-a) * p.x + cos(-a) * p.y,
        p.z);
}

vec3 mirror( vec3 a, vec3 b ) // b must be normalized
{
    return a - (2.0 * dot(a, b)) * b;
}

float positiveSpaceX(in vec3 p) { return -p.x; }
float positiveSpaceY(in vec3 p) { return -p.y; }
float positiveSpaceZ(in vec3 p) { return -p.z; }
float negativeSpaceX(in vec3 p) { return p.x; }
float negativeSpaceY(in vec3 p) { return p.y; }
float negativeSpaceZ(in vec3 p) { return p.z; }

float opUnion( float d1, float d2 ) { return min(d1,d2); }
float opSubtraction( float d1, float d2 ) { return max(-d1,d2); }
float opIntersection( float d1, float d2 ) { return max(d1,d2); }

float sdPlane( vec3 p )
{
	return p.y;
}

float sdSphere( vec3 p, float s )
{
    return length(p)-s;
}

float sdCircle( vec2 p, float s )
{
    return length(p)-s;
}

vec3 divComponents(in vec3 v, in vec3 factors)
{
    return vec3(v.x / factors.x, v.y / factors.y, v.z / factors.z);
}

vec2 divComponents(in vec2 v, in vec2 factors)
{
    return vec2(v.x / factors.x, v.y / factors.y);
}

float sdBox( vec3 p, vec3 b )
{
    b = b * 0.5;
    p = p - b;
    vec3 d = abs(p) - b;
    return min(max(d.x,max(d.y,d.z)),0.0) + length(max(d,0.0));
}

float distSqrd(in vec2 v) { return dot(v, v); }

bool onRhs(PolySeg seg, in vec2 p)
{
    vec2 ortho = vec2(seg.dir.y, -seg.dir.x);
    float scalarProj = dot(p - seg.start, ortho);
    return scalarProj > 0.0;
}

float sdPolygon(in vec2 p, int start, int end)
{
    PolySeg nearest;
    float lowest_d, sp;
    bool first = true;

    for (int i = start; i < end; ++i)
    {
        PolySeg seg = segs[i];
        vec2 q = p - seg.start;
        float scalarProj = max(0.0, min(seg.size, dot(q, seg.dir)));
        float d = distSqrd(q - scalarProj * seg.dir);

        if (!first && d > lowest_d) continue;

        first = false;
        lowest_d = d;
        nearest = seg;
        sp = scalarProj;
    }

    float halfSize = 0.5 * nearest.size;

    PolySeg segA = segs[sp < halfSize ? nearest.prev : nearest.index],
            segB = segs[sp < halfSize ? nearest.index : nearest.next];

    bool inside = onRhs(segA, segB.end)
        ? onRhs(segA, p) && onRhs(segB, p) // convex
        : onRhs(segA, p) || onRhs(segB, p); // concave

    return sqrt(lowest_d) * (inside ? -1.0 : 1.0);
}

//------------------------------------------------------------------

#define ZERO (min(iFrame,0))

//------------------------------------------------------------------

// http://iquilezles.org/www/articles/boxfunctions/boxfunctions.htm
vec2 iBox( in vec3 ro, in vec3 rd, in vec3 rad )
{
    vec3 m = 1.0/rd;
    vec3 n = m*ro;
    vec3 k = abs(m)*rad;
    vec3 t1 = -n - k;
    vec3 t2 = -n + k;
    return vec2( max( max( t1.x, t1.y ), t1.z ),
                 min( min( t2.x, t2.y ), t2.z ) );
}

const float maxHei = 0.8;

float scene(in vec3 pos);

vec2 map( in vec3 pos )
{
    return vec2(scene(pos), 3.0);
}

vec2 castRay( in vec3 ro, in vec3 rd )
{
    vec2 res = vec2(-1.0,-1.0);

    float tmin = 1.0;
    float tmax = 20.0;

    // raymarch primitives
    vec2 tb = iBox( ro-vec3(0.5,0.4,-0.5), rd, vec3(2.0,/*0.41*/ 2,3.0) ); // todo: bad
    if( tb.x<tb.y && tb.y>0.0 && tb.x<tmax)
    {
        tmin = max(tb.x,tmin);
        tmax = min(tb.y,tmax);

        float t = tmin;
        for( int i=0; i<70 && t<tmax; i++ )
        {
            vec2 h = map( ro+rd*t );
            if( abs(h.x)<(0.0001*t) )
            { 
                res = vec2(t,h.y); 
                 break;
            }
            t += h.x;
        }
    }

    return res;
}

float shadow( in vec3 ro, in vec3 rd, float mint, float maxt )
{
    for( float t=mint; t<maxt; )
    {
        float h = map(ro + rd*t).x;
        if( h<0.001 )
            return 0.0;
        t += h;
    }
    return 1.0;
}

// http://iquilezles.org/www/articles/normalsSDF/normalsSDF.htm
vec3 calcNormal( in vec3 pos )
{
    // inspired by tdhooper and klems - a way to prevent the compiler from inlining map() 4 times
    vec3 n = vec3(0.0);
    for( int i=ZERO; i<4; i++ )
    {
        vec3 e = 0.5773*(2.0*vec3((((i+3)>>1)&1),((i>>1)&1),(i&1))-1.0);
        n += e*map(pos+0.0005*e).x;
    }
    return normalize(n);
}

float calcAO( in vec3 pos, in vec3 nor )
{
	float occ = 0.0;
    float sca = 1.0;
    for( int i=ZERO; i<5; i++ )
    {
        float hr = 0.01 + 0.12*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.95;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 ) * (0.5+0.5*nor.y);
}

vec3 render( in vec3 ro, in vec3 rd, in vec3 rdx, in vec3 rdy )
{ 
    vec3 col = vec3(0.7, 0.7, 0.9) - max(rd.y,0.0)*0.3;
    vec2 res = castRay(ro,rd);
    float t = res.x;
    float m = res.y;
    if( m>-0.5 )
    {
        vec3 pos = ro + t*rd;
        vec3 nor = (m<1.5) ? vec3(0.0,1.0,0.0) : calcNormal( pos );
        vec3 ref = reflect( rd, nor );

        // material
        col = 0.2 + 0.18*sin( m*2.0 + vec3(0.0,0.5,1.0) );

        // lighting
        float occ = calcAO( pos, nor );
        vec3  lig = normalize( vec3(-0.5, 0.4, -0.6) );
        vec3  hal = normalize( lig-rd );
        float amb = sqrt(clamp( 0.5+0.5*nor.y, 0.0, 1.0 ));
        float dif = clamp( dot( nor, lig ), 0.0, 1.0 );
        float bac = clamp( dot( nor, normalize(vec3(-lig.x,0.0,-lig.z))), 0.0, 1.0 )*clamp( 1.0-pos.y,0.0,1.0);
        float dom = smoothstep( -0.2, 0.2, ref.y );
        float fre = pow( clamp(1.0+dot(nor,rd),0.0,1.0), 2.0 );

        dif *= shadow( pos, lig, 0.02, 2.5 );
        dom *= shadow( pos, ref, 0.02, 2.5 );

        float spe = pow( clamp( dot( nor, hal ), 0.0, 1.0 ),16.0)*
                    dif *
                    (0.04 + 0.96*pow( clamp(1.0+dot(hal,rd),0.0,1.0), 5.0 ));

        vec3 lin = vec3(0.0);
        lin += 3.80*dif*vec3(1.30,1.00,0.70);
        lin += 0.55*amb*vec3(0.40,0.60,1.15)*occ;
        lin += 0.85*dom*vec3(0.40,0.60,1.30)*occ;
        lin += 0.55*bac*vec3(0.25,0.25,0.25)*occ;
        lin += 0.25*fre*vec3(1.00,1.00,1.00)*occ;
        col = col*lin;
        col += 7.00*spe*vec3(1.10,0.90,0.70);

        col = mix( col, vec3(0.7,0.7,0.9), 1.0-exp( -0.0001*t*t*t ) );
    }

	return vec3( clamp(col,0.0,1.0) );
}

mat3 setCamera( in vec3 ro, in vec3 ta, float cr )
{
    vec3 cw = normalize(ta-ro);
    vec3 cp = vec3(sin(cr), cos(cr),0.0);
    vec3 cu = normalize( cross(cw,cp) );
    vec3 cv =          ( cross(cu,cw) );
    return mat3( cu, cv, cw );
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 mo = iMouse.xy/iResolution.xy;
    float time = 15.0 + iTime*1.5;

    // camera	
    vec3 ta = vec3( 0.5, -0.4, -0.5 );
    vec3 ro = ta + vec3( 4.5*cos(0.1*time + 6.0*mo.x), 1.0 + 2.0*mo.y, 4.5*sin(0.1*time + 6.0*mo.x) );
    // camera-to-world transformation
    mat3 ca = setCamera( ro, ta, 0.0 );

    vec3 tot = vec3(0.0);
#if AA>1
    for( int m=ZERO; m<AA; m++ )
    for( int n=ZERO; n<AA; n++ )
    {
        // pixel coordinates
        vec2 o = vec2(float(m),float(n)) / float(AA) - 0.5;
        vec2 p = (2.0*(fragCoord+o)-iResolution.xy)/iResolution.y;
#else
        vec2 p = (2.0*fragCoord-iResolution.xy)/iResolution.y;
#endif

        // ray direction
        vec3 rd = ca * normalize( vec3(p,2.5) );

         // ray differentials
        vec2 px = (2.0*(fragCoord+vec2(1.0,0.0))-iResolution.xy)/iResolution.y;
        vec2 py = (2.0*(fragCoord+vec2(0.0,1.0))-iResolution.xy)/iResolution.y;
        vec3 rdx = ca * normalize( vec3(px,2.5) );
        vec3 rdy = ca * normalize( vec3(py,2.5) );

        // render	
        vec3 col = render( ro, rd, rdx, rdy );

        // gamma
        col = pow( col, vec3(0.4545) );

        tot += col;
#if AA>1
    }
    tot /= float(AA*AA);
#endif

    fragColor = vec4( tot, 1.0 );
}
