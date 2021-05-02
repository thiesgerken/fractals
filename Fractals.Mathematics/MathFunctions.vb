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

Imports System.Drawing
Imports System.Text
Imports System.Globalization

''' <summary>
''' Exposes some useful functions
''' </summary>
Public Class MathFunctions
#Region "Properties"

    ''' <summary>
    ''' Gets or sets the value to use as smallest value > 0.
    ''' </summary>
    ''' <value>The epsilon.</value>
    Public Shared Property Epsilon As Double = 0.00000001

#End Region

#Region "Methods"

    ''' <summary>
    ''' Uses Newton's method to get the roots of a given complex function
    ''' </summary>
    ''' <param name="func">The function.</param>
    ''' <param name="derivative">The derivative.</param>
    ''' <returns></returns>
    Public Shared Function GetRoots(ByVal func As String, ByVal derivative As String) As List(Of ComplexNumber)
        Return GetRoots(func, derivative, New Rectangle(-2, -2, 4, 4))
    End Function

    ''' <summary>
    ''' Gets the roots.
    ''' </summary>
    ''' <param name="f">The function.</param>
    ''' <param name="f1">The derivative of the function.</param>
    ''' <returns></returns>
    Public Shared Function GetRoots(ByVal f As MathFunction(Of ComplexNumber, ComplexNumber), ByVal f1 As MathFunction(Of ComplexNumber, ComplexNumber)) As List(Of ComplexNumber)
        Return GetRoots(f, f1, New Rectangle(-2, -2, 4, 4))
    End Function

    ''' <summary>
    ''' Gets the roots of a given function.
    ''' </summary>
    ''' <param name="func">The function.</param>
    ''' <param name="derivative">The derivative of the function.</param>
    ''' <param name="region">The region to search in.</param>
    ''' <returns></returns>
    Public Shared Function GetRoots(ByVal func As String, ByVal derivative As String, ByVal region As Rectangle) As List(Of ComplexNumber)
        Return GetRoots(MathFunction(Of ComplexNumber, ComplexNumber).Create(func), MathFunction(Of ComplexNumber, ComplexNumber).Create(derivative))
    End Function

    ''' <summary>
    ''' Gets the roots of a given function f.
    ''' </summary>
    ''' <param name="f">The function</param>
    ''' <param name="f1">The derivative of f.</param>
    ''' <param name="region">The region to search in.</param>
    ''' <returns></returns>
    Public Shared Function GetRoots(ByVal f As MathFunction(Of ComplexNumber, ComplexNumber), ByVal f1 As MathFunction(Of ComplexNumber, ComplexNumber), ByVal region As Rectangle) As List(Of ComplexNumber)
        'Take some of the pixels and compute the value they are converging to (mini-newton)

        Dim maxIterations = 60
        Dim stretch = 20
        Dim roots As New List(Of ComplexNumber)

        For x = region.Left * stretch To region.Right * stretch
            For y = region.Top * stretch To region.Bottom * stretch
                Dim z = New ComplexNumber(x / stretch, y / stretch)
                Dim z1 = z + New ComplexNumber(1, 1)
                Dim n = 0
                Dim pos = z

                While Not Math.Abs(ComplexNumber.Abs(z) - ComplexNumber.Abs(z1)) < Epsilon And n < maxIterations
                    z1 = z

                    If f1(z) = 0 Then z = 0
                    z = z - (f(z) / f1(z))

                    n += 1
                End While

                If n < maxIterations And ComplexNumber.Abs(f(z)) < Epsilon Then
                    'converges to a root
                    If Not roots.Contains(z) Then
                        roots.Add(z)
                    End If
                End If
            Next
        Next

        'only take "unique" roots using the assumption that 2 roots differ in delta > epsilon
        Dim uniqueRoots As New List(Of ComplexNumber)

        For Each r In roots
            Dim isUnique = True

            For Each ur In uniqueRoots
                If ComplexNumber.Abs(ur - ComplexNumber.Round(r, 15)) < Epsilon Then
                    isUnique = False
                    Exit For
                End If
            Next

            If isUnique Then uniqueRoots.Add(ComplexNumber.Round(r, 15))
        Next

        Return uniqueRoots
    End Function

    ''' <summary>
    ''' Gets the derivative of a polynom.
    ''' </summary>
    ''' <param name="polynom">The polynom to calculate the derivative of.</param>
    ''' <returns>the derivative of the polynom</returns>
    Public Shared Function GetDerivative(ByVal polynom As String) As String
        Dim sums As New List(Of String)

        If Not polynom.StartsWith("-") Then polynom = "+" & polynom

        polynom = polynom.Replace("+", "|+")
        polynom = polynom.Replace("-", "|-")

        For Each sum In polynom.Split(New Char() {"|"c}, StringSplitOptions.RemoveEmptyEntries)
            Dim sign = 1

            If sum.StartsWith("-") Then sign = -1
            If sum.StartsWith("+") Then sign = 1

            sum = sum.Substring(1)

            sums.Add(DerivateSum(sum, sign))
        Next

        'build derivative string
        Dim sb As New StringBuilder
        Dim first As Boolean = True
        For Each sum In sums
            If first Then
                If sum.StartsWith("+") Then sum = sum.Substring(1)
                first = False
            End If

            sb.Append(sum)
        Next

        Return sb.ToString
    End Function

    ''' <summary>
    ''' Gets the function with roots at the specified locations.
    ''' </summary>
    ''' <param name="roots">The roots of the desired function.</param>
    ''' <returns></returns>
    Public Shared Function GetFunction(ByVal roots As List(Of ComplexNumber), ByVal a As Double) As String
        'To get the normal polynomial form (ax^n + bx^(n-1) + ... ) we start with f(x)=1 and multiply this with every (x-rn).
        'a function with roots r1, r2, ..., rn is obtained through:
        'f(x)=(x-r1)(x-r2)...(x-rn)

        Dim functionParts As New List(Of List(Of String)) 'List of Factors in a list of Summands 

        Dim init As New List(Of String)
        init.Add(a.ToString) 'Initialize with the desired seed (there is an infinite amount of functions having the required roots)

        functionParts.Add(init)

        'get the sums
        For Each root In roots
            Dim newFunctionParts As New List(Of List(Of String))

            For Each sum In functionParts
                Dim sum1 As New List(Of String)
                sum1.AddRange(sum)
                sum1.Add("x")

                Dim sum2 As New List(Of String)
                sum2.AddRange(sum)
                sum2.Add((-root.RealPart).ToString(CultureInfo.InvariantCulture))

                Dim sum3 As New List(Of String)
                sum3.AddRange(sum)
                sum3.Add((-root.ImaginaryPart).ToString(CultureInfo.InvariantCulture))
                sum3.Add("i")

                newFunctionParts.Add(sum1)
                newFunctionParts.Add(sum2)
                newFunctionParts.Add(sum3)
            Next

            functionParts = newFunctionParts
        Next

        'expand produces terms like "1*1*-1*5*-3*x*x*x*i*i", so we have to clean this to something like "15*i^2*x^3"
        ' and find Sums that cab ve combined (like 3ix^3 and -3ix^3 and 4ix^3)
        Dim summands As New Dictionary(Of String, Double)

        'clean the sums
        For i As Integer = 0 To functionParts.Count - 1
            If Not functionParts(i).Contains("0") Then
                Dim xcount As Integer = 0
                Dim icount As Integer = 0

                Dim factor As Double = 1

                For ii As Integer = 0 To functionParts(i).Count - 1
                    If functionParts(i)(ii) = "x" Then
                        xcount += 1
                    ElseIf functionParts(i)(ii) = "i" Then
                        icount += 1
                    ElseIf Double.TryParse(functionParts(i)(ii), New Integer) Then
                        factor *= Double.Parse(functionParts(i)(ii), CultureInfo.InvariantCulture)
                    End If
                Next

                While icount >= 2
                    factor *= -1
                    icount -= 2
                End While

                Dim summand As String = ""

                If icount = 1 Then
                    summand &= "i"

                    If xcount > 0 Then
                        summand &= "*"
                    End If
                End If

                If xcount = 1 Then
                    summand &= "x"
                ElseIf xcount > 1 Then
                    summand &= "x^" & xcount
                End If
                If summands.ContainsKey(summand) Then
                    summands(summand) += factor
                Else
                    summands.Add(summand, factor)
                End If
            End If
        Next

        'build output
        Dim sb As New StringBuilder
        Dim firstSum As Boolean = True
        For Each sum In summands
            If Not sum.Value = 0 Then
                If Not firstSum And sum.Value > 0 Then sb.Append("+")
                firstSum = False

                If (sum.Value <> 1 Or sum.Key = "") And Not (sum.Value = -1 And sum.Key <> "") Then
                    sb.Append(sum.Value.ToString(CultureInfo.InvariantCulture))

                    If sum.Key <> "" Then
                        sb.Append("*")
                    End If
                End If

                If sum.Key <> "" Then
                    If sum.Value = -1 Then sb.Append("-")

                    sb.Append(sum.Key)
                End If
            End If
        Next

        Return sb.ToString
    End Function

    ''' <summary>
    ''' Derivates a part of a polynom (e.g. 3*x^4)
    ''' </summary>
    ''' <param name="sum">The summand to calculate the derivative of.</param>
    ''' <param name="sign">The sign of the summand.</param>
    ''' <returns></returns>
    Private Shared Function DerivateSum(ByVal sum As String, ByVal sign As Integer) As String
        Dim factor As Double = 1
        Dim exponent As Integer = 1
        Dim strfactor As String = ""
        Dim containsI As Boolean = False

        If sum.Contains("x") Then
            If Not sum.StartsWith("x") Then
                strfactor = sum.Split(New String() {"*x"}, StringSplitOptions.RemoveEmptyEntries)(0)
            End If

            If Not sum.EndsWith("x") Then
                Dim strexponent = sum.Split(New String() {"x^"}, StringSplitOptions.RemoveEmptyEntries)

                exponent = CInt(strexponent(strexponent.Length - 1))
            End If
        Else
            'Constant!
            Return ""
        End If

        If strfactor.Contains("i") Then 'there can only be 0 or 1 i because 2 i's would be i^2=-1
            strfactor = strfactor.Replace("i", "")
            containsI = True
        End If

        strfactor = strfactor.Replace("*", "")

        If strfactor <> "" Then factor = Double.Parse(strfactor, CultureInfo.InvariantCulture)

        If factor = 0 Then Return ""

        factor *= exponent * sign
        exponent -= 1

        Dim derivate As String = ""

        If factor > 0 Then derivate = "+"

        If factor = 1 And Not exponent = 0 Then
        ElseIf factor = -1 Then
            derivate &= "-"
        Else
            derivate &= factor.ToString(CultureInfo.InvariantCulture)
        End If

        If containsI Then
            If factor <> 1 Then
                derivate &= "*"
            End If
            derivate &= "i"
        End If

        If exponent = 1 Then
            If factor <> 1 Or containsI Then
                derivate &= "*"
            End If
            derivate &= "x"
        ElseIf exponent > 1 Then
            If factor <> 1 Or containsI Then
                derivate &= "*"
            End If
            derivate &= "x^" & exponent
        End If

        Return derivate
    End Function

#End Region
End Class
