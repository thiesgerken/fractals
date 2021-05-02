' Copyright (C) 2010-2012 Thies Gerken

' This file is part of Fractals.

' Fractals is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.

' Fractals is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.

' You should have received a copy of the GNU General Public License
' along with Fractals. If not, see <http://www.gnu.org/licenses/>.

Public Class Fraction
#Region "Fields"

    Private _denominator As Integer
    Private _numerator As Integer

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the numerator of the fraction.
    ''' </summary>
    ''' <value>The numerator.</value>
    Public Property Numerator As Integer
        Get
            Return _numerator
        End Get
        Set(ByVal value As Integer)
            _numerator = value
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the denominator of the fraction.
    ''' </summary>
    ''' <value>The denominator.</value>
    Public Property Denominator As Integer
        Get
            Return _denominator
        End Get
        Set(ByVal value As Integer)
            _denominator = value
        End Set
    End Property

#End Region

#Region "Constructors"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Fraction" /> class.
    ''' </summary>
    Public Sub New()
        Me.Denominator = 1
        Me.Numerator = 1
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Fraction" /> class.
    ''' </summary>
    ''' <param name="value">The value.</param>
    Public Sub New(ByVal value As Double, ByVal cancel As Boolean)
        Dim dummy As Fraction = value
        Me.Numerator = dummy.Numerator
        Me.Denominator = dummy.Denominator

        If cancel Then Me.Cancel()
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Fraction" /> class.
    ''' </summary>
    ''' <param name="value">The value.</param>
    Public Sub New(ByVal value As Double)
        Me.New(value, True)
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Fraction" /> class.
    ''' </summary>
    ''' <param name="numerator">The numerator.</param>
    ''' <param name="denominator">The denominator.</param>
    Public Sub New(ByVal numerator As Integer, ByVal denominator As Integer, ByVal cancel As Boolean)
        Me.Numerator = numerator
        Me.Denominator = denominator

        If cancel Then Me.Cancel()
    End Sub

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Fraction" /> class.
    ''' </summary>
    ''' <param name="numerator">The numerator.</param>
    ''' <param name="denominator">The denominator.</param>
    Public Sub New(ByVal numerator As Integer, ByVal denominator As Integer)
        Me.New(numerator, denominator, True)
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Calculates the GCD of two given numbers.
    ''' </summary>
    ''' <param name="a">A.</param>
    ''' <param name="b">The b.</param>
    ''' <returns></returns>
    Private Shared Function CalculateGcd(ByVal a As Integer, ByVal b As Integer) As Integer
        If b = 0 Then
            Return a
        Else
            Return CalculateGcd(b, a Mod b)
        End If
    End Function

    ''' <summary>
    ''' Cancels this instance.
    ''' </summary>
    Public Sub Cancel()
        Dim gcd = CalculateGcd(Me.Numerator, Me.Denominator)

        Me.Numerator = CInt(Me.Numerator / gcd)
        Me.Denominator = CInt(Me.Denominator / gcd)
    End Sub

    ''' <summary>
    ''' Returns a <see cref="System.String" /> that represents this instance.
    ''' </summary>
    ''' <param name="separator">The separator.</param>
    ''' <returns>
    ''' A <see cref="System.String" /> that represents this instance.
    ''' </returns>
    Public Overloads Function ToString(ByVal separator As Char) As String
        Return Me.Numerator & " " & separator & " " & Me.Denominator
    End Function

    ''' <summary>
    ''' Returns a <see cref="System.String" /> that represents this instance.
    ''' </summary>
    ''' <returns>
    ''' A <see cref="System.String" /> that represents this instance.
    ''' </returns>
    Public Overrides Function ToString() As String
        Return Me.ToString("/"c)
    End Function

#End Region

#Region "Operators"

    ''' <summary>
    ''' Implements the operator =.
    ''' </summary>
    ''' <param name="frac1">The frac1.</param>
    ''' <param name="frac2">The frac2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator =(ByVal frac1 As Fraction, ByVal frac2 As Fraction) As Boolean
        frac1.Cancel()
        frac2.Cancel()

        If frac1.Denominator <> frac2.Denominator Then Return False
        If frac1.Numerator <> frac2.Numerator Then Return False

        Return True
    End Operator

    ''' <summary>
    ''' Implements the operator &lt;&gt;.
    ''' </summary>
    ''' <param name="frac1">The frac1.</param>
    ''' <param name="frac2">The frac2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator <>(ByVal frac1 As Fraction, ByVal frac2 As Fraction) As Boolean
        Return Not frac1 = frac2
    End Operator

    ''' <summary>
    ''' Implements the operator *.
    ''' </summary>
    ''' <param name="frac1">The frac1.</param>
    ''' <param name="frac2">The frac2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator *(ByVal frac1 As Fraction, ByVal frac2 As Fraction) As Fraction
        Return New Fraction(frac1.Numerator * frac2.Numerator, frac1.Denominator * frac2.Denominator)
    End Operator

    ''' <summary>
    ''' Implements the operator /.
    ''' </summary>
    ''' <param name="frac1">The frac1.</param>
    ''' <param name="frac2">The frac2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator /(ByVal frac1 As Fraction, ByVal frac2 As Fraction) As Fraction
        Return frac1 * New Fraction(frac2.Denominator, frac2.Numerator)
    End Operator

    ''' <summary>
    ''' Implements the operator +.
    ''' </summary>
    ''' <param name="frac1">The frac1.</param>
    ''' <param name="frac2">The frac2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator +(ByVal frac1 As Fraction, ByVal frac2 As Fraction) As Fraction
        Return New Fraction(frac1.Numerator * frac2.Denominator + frac2.Numerator * frac1.Denominator, frac1.Denominator * frac2.Numerator)
    End Operator

    ''' <summary>
    ''' Implements the operator -.
    ''' </summary>
    ''' <param name="frac1">The frac1.</param>
    ''' <param name="frac2">The frac2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator -(ByVal frac1 As Fraction, ByVal frac2 As Fraction) As Fraction
        Return frac1 + New Fraction(frac2.Numerator * -1, frac2.Denominator)
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="x">The x.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Widening Operator CType(ByVal x As Double) As Fraction
        x = Math.Round(x, 3)

        Dim integralpart As Integer = CInt(Math.Truncate(x))

        If integralpart = x Then Return New Fraction(CInt(integralpart), 1)

        Dim digits As Integer = 0

        While x <> integralpart
            x *= 10
            integralpart = CInt(Math.Truncate(x))
            digits += 1
        End While

        Return New Fraction(CInt(integralpart), CInt(10 ^ digits))
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="frac">The frac.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Narrowing Operator CType(ByVal frac As Fraction) As Double
        Return frac.Numerator / frac.Denominator
    End Operator

#End Region
End Class
