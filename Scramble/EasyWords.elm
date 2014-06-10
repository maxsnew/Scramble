module Scramble.EasyWords (max, words) where

import Scramble.Trie as Trie

max   = 1001
fromJust (Just x) = x
words = fromJust . Trie.decode <| "0a0a0a1;,h1e0d1;;,s1;;,l1s1;;,s1;;,c0a0d1;,l0e1;;,n0a1;;,r0a1;,i1;;;,e1d1;;,h1a0f0e1;;,p0e1;;,r1;;,e1d1;,e0r1;;,n0e1;;,r1;;,i0e0r1;;,r0a1;;;,k0a0n1;;;,r0a0s1;;,e0e1;;;;;,d0a0d1;,r1;;,c1;,e1a0d1;;,n1a0s0e1;;;;;,h0e0r0e1;;;;,p1;,s1;;,e0d0e0s1;;;,f0a0l0d1;;;;,s1;;,f0a0c0e1d1;;;,r1;;,d1;,e0a0r1d1;,e0d1;;;;;;,g0a1d1a1;;,l1;,r1s1;;,s1;,z0e1d1;;;;;,h0a1;,e0a0d1;;;,i1;,s1a0n1;;;;,k0a1n1;;,e1d1;;,n0e0e1;;;,r0a1;,e1;;;,l0a1c0h0a1h1;;;;,d0a1;;,e1;,h0e0e1;;;,n1d1;;,p1;,s1;;,d1;,e1e1;;,f1a1;;,k1;,l1;,p1a0c0a1;;;,h0a1;;;,s1;;,n0a1l1;,n1a1;,d0a1;;;,s1;;,d1a1;,e0a0n1;;,d1;;;,e1;,i1;,k0h1;,l0e1d1;;;;,r0e1;;,s1a1e1;;;;,p0a1c0e1;,h0e1;;;,r1;;,e1d1;;,h1;,l1a0c0e1;;;;,r1;;,r0a1c0a1;,e1;,h1e1;;;,d1;,k1e1;;,n0k1;;,s0e1;;;,e1d1;;,f1;,k1;,l0e1;;,r1h0a1;;;,s1e1;;,x1;;,s0a0l0e1;;;,e1a1;;,g1a0r0d1;;;,d1;;,h1c0a0n1;;;,e0d1;,n1;,r1a0h1;;;;,i0e0r1;;,n0e1;;;,l0e0r1;;;;;,z0a0n1;;;;,c0a0c0a1s1;;,e1;,h0a0z0a1;;;,e1d1;;,i0l0a1;;,n0a1;;;;;,d1e1e1;,r1a0s1;;,e1;;,s1;;,s1;;,e0c0a1l1;;;;,f1e1;,h1;;,g1;,h0i0e0r1s1;;;,z1;;;,k0e1d1;;;,l1a0d0e1;;;,f1;,k1e0d1;;;,l1e0d1;;;,p1;,s1;,x1;;,n1a0l1e1d1;,r1;;,i1;,l0e0d1;;;;,r0d1;,i1;;;,d1;,e1d1;,l1;,r1;;,k1;,s1;;,p1e1d1;;,h1;;,r1d1e0d1;;;,e1d1;,r1;,x1;;,f1;,i0e0d1;;;,k1e0d1;;;,l1e1;;,p1e1d1;;;,r1e0e1;;;,s1e1;;;,s0e1d1;;,h1e0d1;,e0n1;;,l1;,r1;;,i0e0r1;;;;;,z0a1;;;,d0f1;,g1;;,e0a0s0e1d1;;;;,c0a1l1;;;,d0a0r1e0d1;;,s1;;;,e1d1;,n0s1;;,r1s1;;,s1;;;,e1;;,h0a1a1;,c0e1;;,d1a0r1;;,s1;;,f0e1d1;;;,g0a0l1;,n1;;;,h0a0r1;;;,l1a0h1;;,d0e0e1;;;,k1e0d1;;;;,n1k1;;,p1e1d1;;;,r1a1c1;,d0e1;;;,d1;,e1d1;,r1;;,k1e0d1;;;,r1e1d1;;;,s1;;,s0e1d1;;;,z0a0n1;;;;,e1d0a0r1;;,e0r1s1;;;;,e1r1s1;;;,i0r1;;,l0a1e1;,s1;;,p1;;,n0a1;;,r1e1;,n0a1;;;,z1;;,i1e0l1s1;;,n1;;,h1;,l1e1;;,n1a1s1;;,e1d1;;,k1e0d1;;;,s1e1d1;;;;,r0k1e0d1;;;,p1e0d1;;;;;,n1;,s1;;;,d0a0c0e1;,h0a1s1;;,s1;;;,d1a1p1;,s1;;,e1;,s1;;,e1d0a0l1;;;;,g1s1;;,h1l1s1;;,s1;;,k1;,l1a0n1;;,e1d0h1s1;;;,r1;;,f1;,i1;,k1;,l0e1;;,s1;;,n1a1e1;;,d1a1;;,e1;,k1e1;;;,p1h0n0e1;,i1;;;;,r1a0c1;;,d1;,e1d1;,r1;;,i1;,k1e0d1;;;,r1;;,s1e1;,h1e0d1;,e1n1;;,l1;,r1;;,i1e0r1;;;;;,z0e1d1;;;;,c0a1;;,e0a1d1e0n1s1;;,r1;;,h0a0n0d1;;;;,s1;;,f1;,l1e0r1;;,s1;;,n1e0d1;,r1;;,s1;;,r1e1r1;;,i0e1;;,s1;;,s1h1e0d1;;;;;,c1a0d1a0l1;;,e1s1;;;,f1;,h0e0d0r0a1;;;;;,l1s1;;,n1a0l1;;,e1;,i1;;,p1;,r0d1;,e1;;;,d1;,e0a0s0e1d1;;;;,d0e1;;;;,d0a1l1;,n0s1;;;;,e1d1e0d1;,e0d1;;;,s1;;,r1s1;;;,f1a0c0e1d1;;;,d0e1;;;,l0e0x1;;;;,h0a0c0h0e1;;;;;,i1l1s1;;,n0k1;;;,k0a0r0e1;;;,l0e1;;;,l1a0c0e1;;;,e1;,f1;,h0i1;;,i1;,l1a1;,s1;;,p0h1;;,s1;;,n1e1;,s1e1;;;,p1a1;,l0a0c0e1;;;;,r1;;,r1a0h1;;,e1;,f1;,k1;,n1;;,s1a0n0d1e0d1;;;;;;,x1;;,h0a1k1;,l1s1;;,n1;;,e0r0i1;;;;,k0g1;,l1;;,r0a0d1;,g1a0d0e1d1;;;;,s1;;,k0e1;;,n0k1;;,p0e1d1;;;;,e0e1d1;;;;,s0r1i1;;;;,e0a0c0h1;;,d1;,n1;,r1a0c0h0e1;;;;,e0d1;;,h0e0a0d1;;;;,l1;,s1h1;;;,s0e1d1;;;;,c0a0d1;,n0d0a1;;;;,h0a0r1d1;;;,e1a1;,d1;;;;,d0e0a1;,n1;;,g0a0r1;;;,h1s1;;,p1;,s1;;,e0l1s1;;,n1;,r1;;,f0l1;;,i0l0a1;;,r1e1;;;,k0e1d1;;;,l0a1n1d1;;;,d1;,f1;,h0i1;;,i1;,k1;,l1a0n1;;,e1;,s1;;,n0e1;;,s1e1;;;,n0d1e1d1;;,h0a0n0d1;;;;;,e0d1;;,f0a0c0e1d1;;;;;,l1;,s1e1;,h0a0d0e1;;;;;;,p0a1;,h0a1h1;;,e0d0r0a1;;;;;;,r0a1d0e1;;,s1e1d1;;;;,d1;,e1n0a0c0h1;;;;,r1;;,f1;,i0e1;;,k1e1;;,n1e1;,s1;;,r1a0n0d1;;;,e0d1;;,s1;;,s1e1;,h1;;;,s0d1;,e1;,h0i0n1;;;,n0e1;;;,x0e0d0e1;;;,h0a0l0e1d1;;;;;,l0a1;;,r1;;,z0a0n1;;;;,f0a0c1a0d0e1d1;;;;,e1d1;;;,d1e1d1;,n1;,r1;;;,e1;,g1;,l0l1;,x1;;,n1d1;,e1;;,r1d1e1d1;;,h1;;,e1d1;,r1;;,l1e1;;;,z0e1d1;;;;,e0a0l1;,r1e0d1;,r1;;;,z0e1d1;;;;,d1a0r0i0e1;;;;;;,f0a1;;,h0r0e0r1;;;;,l0a0g1;,n1;;,d1;,e0d1;,e1;,x1;;,i0e0d1;;;,l1;;,r0a1c0h0e1;;;,e1;,g1;;,e0d1;,e1d1;,h0a0n0d1;;;;;;,i0e0d1;;;;;,g0a0d1a0e0a1;;;,e1a0n1;;,r1;;,s1;;,e1a1;,d1;;,g1a1;,s1;;,l1a1h1;;,e1e1;;,i1;,l1e0d1;;;,p1;,s1;;,n1e1;;,p1;,r1a1d1;;,d1e1;;,e1h1;;,l0e1;;,r0e0d1;;;,s1;;,s1h1e0d1;,r1;;;;,z1e1d1;,e1;,l1;,r1;;,i1;;;,d0s1;;,p0a0d1;;,d1;,h1;;;,h0a0a0r1;;,c0h0e1;;;,d1a0d0a1;;,l1;;,e1d1;,s1;;;,e1c1;,d1;,s1;;,f1;,g1a0d0a1;;,r1;;,s1;;,h1a1;,s1;;,k1e1;;,l0a1;,e1d1;,r1;;,f1h0e0a0d0e0d1;;;;;;;,k0e1;;,l1;,p1;,s1e1;;;,n1d1c0a0r1;;;,e0d1;;;,k1e0d1;;,l0e1;;;,s1e1;;;,p1;,r0a0c0e1;;;,d1h0e0a0d1;;;;;,e1d1;;,k1e0d1;;;,l1e1d1;;;,p1e0d1;;;,r1e1;;,s0h1e0r1;;;;;,s1h1e0d1;,r1;;,h0e0a0d1;;;;;,n1;;,z0a0n1;;,e1d1;,l1;,r1;;,l0e1;;;;,e0a0d1a0c0h0e1;,i0e0r1;;;;;;,c0a0p1;;;,e0d1;,n0d1;;,r1s1;;;;;,d1e1r1s1;;;;,e1d1;,l1s1;;,r1;,z0e1;;;,h1s1;;,i1l1s1;;,r1s1;;;,l1a0s1;;,d1;,e1;,l1h0a0g1;;;,s1;;,p1e0d1;;;;,n1a0d1;;,d1;,s1;;,r1a1;,d1;,e1;,l1s1;;,n1s1;;,p1;,r1;,s1e1d1;;;;,x1;;,h0d1;;,i0e1d1;,l0a0n0d1;;;;,r0a1;;;,l0a1;,e1;,s0a1h1;;;;,n1e1;,s1;;,r1e1d1;;,s0e1;;;;,l0d1;;,r0s1;;;,i0h0i1;,p1;,s1;;,l0e1;,k1a1;;;,n0f1a0c0e1;;,n0d1;;;;,k1e0d1;;;,s1e0a1;;,h0a0d0e1;;;;;;,r0a1d0e1s1;;;,n1;;,e1d1;;,k1e0d1;;;,p0e1;;,s1;;;,k0a0c0h0i0n1;;;;,d0e0i0n1;;;;,e1;,l1e1;,i1;;,n1d1e1;;,e1h1;;;,r0r0e0e1;;;;,z0i1;;;,e0a1c0h1;;;,d1;;,h0a1n1;,r1;;,e0d0a1h1;;;,l0l0a1;;;;,i1r1k0a1h1;;;;;;,l0a0n1;;;,n0a0g1;,r1k1;,l1e1;;,r0e0d1;;;;;,e0e1d1;;;;,r0a1n1;;;;,l0a0c1e1d1;;,h0e1;;;,d1e1d1;,r1;,s1;;,s1;;,g1s1;;,h1;,k1e1d1;;,h1;;,l0l1e0d1;;;;,n1d1e1;;,k1;;,p1;,s1e1d1;;,h1e0d1;,r1;;;;;,e0d1a1;,e1;;,e1d1;;,h0r1s1;;;,i1;,n0a0d1;;,s1e1d1;;;;,r0p1;;,x1;,z1;;,h0d1;;,i0e1d1;;,l0a1c1;,s1;;,e1;;;,l0e0r1;;;;,n0a0a1;,c0e1;,h1a0n0i1;;;,e1;;;,d1a1;;,e1;,f1;,g1a1;,s1;;,h1;,k1e1d1;;;,l0e1d1;;;,n1a1;,e1;;,p1e1;;,r1d1;,e1;,k1;,r1;;,s0h1;;,z0e1;,i1;;;,e0e1d1;;,i1n1;;,n0e1;;;,i0n0e1;;;;,p0a0c1a1;,e1d1;;;,d1;,h1i1;;,l1e1d1;,r1;;,i1;,l1e0d1;;;,p1e0d1;;;;,n1d1;,e1d1;,l1;;,h0e0a0d1e0d1;;;;;;;,p1e1;;,r1d1e1d1;,e1;;;,e1d1;,l1;,r1;;,i1;,l1e1d1;;;,r1e0d1;;;;;,e0a1c0e1d1;;,h1e0d1;,n1;,r1;;,i0e0r1;;;;;,h0e0n1;;;,l1e0d1;,r1;;;,n1;,r1l1e0d1;;;;;,d1a1l1e0d1;,r1;;,l0e0d1;;;;;,e0e1;;;,p1l0a1;;;;,h0a0n0e1;;,r1e1;;;,e0n0e1;;;,i1l1;,z1;;,r1e0n1;;;;,l0a0c0e1d1;;;,p1;;,e0d1;,x1;;,i1e1d1;;;;,p0a1;,d1;,h1;,l1;,r1;;,r0a0d1;;,e1d1;,e1d1;;,x1;;,i0e1d1;;;,p1;;;,r0a0a0d1;;,c0e1d1;;,h1e1;;;,d1a1r1;;,s1;;,g1a1;,s1;;,h1d0a0r1e0e1;;,i1;;;;;,k0e1d1;;,h1;;,n1a1;,d1;,k1;;,p1e1d1;;,h0e1;;;,r0a1;,e1d1;,r1;;;,s1e1d1;;,h1e0r1;;;;;,e0d1a0n1s1;;,r0e1;;;,c0a0p1;;;,e1e0d1;;;,h0e0a0d1;;;;,r0a0g1;,p0e1;;;;;,e1d1;;,h1a0s0h1e0d1;;;;;,e0a0d1;;;;,i1;,l1a0c0e1d1;;,h0e1;;;,d0e1;;,n0d1;;,p1;;;,n1k1;;,r0a0k0e1;;,n1k1;;;,e0e1;;;,x1;;,f0r0e0e1;;;;,h0a1p0h0e1;;;;,d1;,e1a1;,d0a1e1;,s1;;;,e0n1;;;;,i0e1r1;;;,l0e1;;;,s0a0c1;,d1e1s1;;,h1e1;;;,e1;,g1s1;;,h1h1;;,l1e1;,l1e1e1;;;,s1e1;;;,n1d1e0d1;;;,e1d1;,r1;;,s1;;,r1d1;,e1e1;;,i1;;,s0h1e0d1;;;;;,d0s1;;,e0a1h1;,l1e0d1;,r1;;,s1;;,r1e0d1;,r1;;,s1;;,s1h0e0l0l1;;;,i0n0e1;;;;;;,d1a0n1s1;;;;;,g0d1;;,h0a1c0h0l0e1;;;;,d1c0h0e0n1;;;;,e1d1;,r1s1;;,s1;;,s1;;,g1s1;;,h1e0e1n1;;;,i1n1;;,s1;;,l0e1d1;,e1;;,l1;;,n1;,r0d1e0d1;;;,e1d1;,r1;;;;,e1a1;,d1e0r1;;;,e1l1;,n1s1;;,r1s1;;;,i0l0a1s1;;;;,l0a1h1;;,d1;,l1a0c1;;,s1;;;,n0d1;;,r1d1;;;,h1;,i1e0d1;,l1s1;;,r1s1;;;,h1;,n1e1d1;;,s1;;,r0e1;;;,r1a0d0h1;;,g1;;,e0d1;,e1;;,i1;;;,l0a1d1e1;;,e1;,g1s1;;,s0h1e0d1;,r1;;;;;,d1;,e0d1;,e1;;;,n0a0g1s1;;,s0h1;;;,e0d1;,e1;;;,r0a0c1;;,i1;;,s0e0d1;;;;,x0e0d1;;;,z0a0c1;,d1;,g1s1;;,k1;,p1;;,e0d1;,e1d1;;,l1;,r1;;;"