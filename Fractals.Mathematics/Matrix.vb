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

Public Class Matrix
#Region "Fields"

    Private _inner(,) As Double

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Matrix" /> class.
    ''' </summary>
    ''' <param name="cols">The number of columns.</param>
    ''' <param name="rows">The number of rows.</param>
    Public Sub New(ByVal cols As Integer, ByVal rows As Integer)
        ReDim _inner(cols - 1, rows - 1)
    End Sub

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the value at a specified location in the matrix.
    ''' </summary>
    ''' <value>The value.</value>
    Default Public Property Value(ByVal col As Integer, ByVal row As Integer) As Double
        Get
            Return _inner(col, row)
        End Get
        Set(ByVal value As Double)
            _inner(col, row) = value
        End Set
    End Property

    ''' <summary>
    ''' Gets the column count.
    ''' </summary>
    ''' <value>The column count.</value>
    Public ReadOnly Property Columns As Integer
        Get
            Return _inner.GetLength(0)
        End Get
    End Property

    ''' <summary>
    ''' Gets the row count.
    ''' </summary>
    ''' <value>The row count.</value>
    Public ReadOnly Property Rows As Integer
        Get
            Return _inner.GetLength(1)
        End Get
    End Property

#End Region

#Region "Operators"

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="c">The c.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Widening Operator CType(ByVal c As ComplexNumber) As Matrix
        Dim m As New Matrix(1, 2)
        m(0, 0) = c.RealPart
        m(0, 1) = c.ImaginaryPart

        Return m
    End Operator

    ''' <summary>
    ''' Implements the operator CType.
    ''' </summary>
    ''' <param name="m">The m.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Narrowing Operator CType(ByVal m As Matrix) As ComplexNumber
        If m.Columns <> 1 Or m.Rows <> 2 Then Throw New ArgumentException("This Matrix is no 2d-vector")
        Return New ComplexNumber(m(0, 0), m(0, 1))
    End Operator

    ''' <summary>
    ''' Implements the operator +.
    ''' </summary>
    ''' <param name="m1">The m1.</param>
    ''' <param name="m2">The m2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator +(ByVal m1 As Matrix, ByVal m2 As Matrix) As Matrix
        If m1.Columns <> m2.Columns Or m1.Rows <> m2.Rows Then Throw New ArgumentException("The matrixes must have the same size")

        Dim m As New Matrix(m1.Columns, m1.Rows)

        For x As Integer = 0 To m.Columns - 1
            For y As Integer = 0 To m.Rows - 1
                m(x, y) = m1(x, y) + m2(x, y)
            Next
        Next

        Return m
    End Operator

    ''' <summary>
    ''' Implements the operator -.
    ''' </summary>
    ''' <param name="m1">The m1.</param>
    ''' <param name="m2">The m2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator -(ByVal m1 As Matrix, ByVal m2 As Matrix) As Matrix
        Return m1 + m2 * -1
    End Operator

    ''' <summary>
    ''' Implements the operator *.
    ''' </summary>
    ''' <param name="m">The m.</param>
    ''' <param name="r">The r.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator *(ByVal m As Matrix, ByVal r As Double) As Matrix
        For x As Integer = 0 To m.Columns - 1
            For y As Integer = 0 To m.Rows - 1
                m(x, y) *= r
            Next
        Next

        Return m
    End Operator

    ''' <summary>
    ''' Implements the operator *.
    ''' </summary>
    ''' <param name="m1">The m1.</param>
    ''' <param name="m2">The m2.</param>
    ''' <returns>The result of the operator.</returns>
    Public Shared Operator *(ByVal m1 As Matrix, ByVal m2 As Matrix) As Matrix
        If m1.Columns <> m2.Rows Then Throw New ArgumentException("m1.cols must be m2.rows")

        Dim m As New Matrix(m2.Columns, m1.Rows)

        For x As Integer = 0 To m.Columns - 1
            For y As Integer = 0 To m.Rows - 1
                Dim value As Double = 0

                For k As Integer = 0 To m1.Rows - 1
                    value += m1(k, y) * m2(x, k)
                Next

                m(x, y) = value
            Next
        Next

        Return m
    End Operator

#End Region

#Region "Methods"

    ''' <summary>
    ''' Returns a <see cref="System.String" /> that represents this instance.
    ''' </summary>
    ''' <returns>
    ''' A <see cref="System.String" /> that represents this instance.
    ''' </returns>
    Public Overrides Function ToString() As String
        Dim s As String = ""

        For y As Integer = 0 To Me.Rows - 1
            s &= "("

            Dim first As Boolean = True

            For x As Integer = 0 To Me.Columns - 1
                If Not first Then s &= ","
                first = False

                s &= Me(x, y)
            Next

            s &= ")"

            If y <> Me.Rows - 1 Then s &= Environment.NewLine
        Next

        Return s
    End Function

#End Region
End Class
