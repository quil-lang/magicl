# Notes

1. Initial LAPACK files pulled from F2CL's test suite, cf [https://gitlab.common-lisp.net/f2cl/f2cl/-/tree/master/packages/lapack](https://gitlab.common-lisp.net/f2cl/f2cl/-/tree/master/packages/lapack)

2. Additional LAPACK files pulled from the 3.4.2 release of LAPACK

3. LAPACK code occasionally has calls to subroutines expecting a single character / string of length 1 is made with a longer string, with the assumption that the fortran compiler will smooth this over (e.g. `CALL ZLASR('Right', ...`as opposed to `CALL ZLASR('R', ...)`. F2CL will fail on these; indeed, the latest F2CL release fails on the version of LAPACK which is included in its own test suite.... However, replacing these strings with their length 1 prefix corrects the problem, and I have done this here.

4. Some of the routines (specifically ilazlr, zlarft, zbbcsd, the latter needed for zuncsd) use `EXIT` for breaking out of loops, which is a Fortran 90 feature that F2CL has difficulty with. In all cases, this may be replaced by an explicit check in the loop itself, e.g. from `zbbcsd.f`,

```
      IF  ( IMIN .GT. 1 ) THEN
         DO WHILE( PHI(IMIN-1) .NE. ZERO )
            IMIN = IMIN - 1
            IF  ( IMIN .LE. 1 ) EXIT
         END DO
      END IF
```

may be replaced by

```
      DO WHILE( ( IMIN .GT. 1 ) .AND. ( PHI(IMIN-1) .NE. ZERO ) )
         IMIN = IMIN - 1
      END DO
```

and (from `zlarft.f`)

```
                     DO LASTV = 1, I-1
                        IF( V( LASTV, I ).NE.ZERO ) EXIT
                     END DO
```

may be replaced by

```
                     LASTV = 1
                     DO WHILE( ( LASTV .LT. I ) .AND. ( V( LASTV, I).NE.ZERO ) )
                        LASTV = LASTV + 1
                     END DO
```