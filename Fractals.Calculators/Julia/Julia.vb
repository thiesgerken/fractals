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
Imports Fluent
Imports Fractals.Mathematics
Imports Fractals.Utilities
Imports CommandLineParser

''' <summary>
''' Fractal that renders Julia sets.
''' </summary>
Public Class Julia
    Inherits Fractal

#Region "Enums"

    ''' <summary>
    ''' A set of possible function types
    ''' </summary>
    Public Enum Functions
        ''' <summary>
        ''' z = sin(z) * c
        ''' </summary>
        Sine
        ''' <summary>
        ''' z = cos(z) * c
        ''' </summary>
        Cosine
        ''' <summary>
        ''' z = z^k + c
        ''' </summary>
        Polynomial1
        ''' <summary>
        ''' z = (z - z ^ k) * c
        ''' </summary>
        Polynomial2
        ''' <summary>
        ''' z = e^(|Re(z)| + |Im(z)|i) * c
        ''' </summary>
        Exponential
    End Enum

#End Region

#Region "Private Fields"

    Private _exponent As Double
    Private _constant As ComplexNumber
    Private _type As Functions

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the exponent.
    ''' </summary>
    ''' <value>The exponent.</value>
    Public Property Exponent As Double
        Get
            Return _exponent
        End Get
        Set(ByVal value As Double)
            _exponent = value

            OnPropertyChanged("Exponent")
            OnFractalSettingChanged("Exponent")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the real part of the constant.
    ''' </summary>
    ''' <value>The constant's real part.</value>
    Public Property ConstantRe As Double
        Get
            Return _constant.RealPart
        End Get
        Set(ByVal value As Double)
            _constant = New ComplexNumber(value, _constant.ImaginaryPart)

            OnPropertyChanged("Constant")
            OnFractalSettingChanged("Constant")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the imaginary part of the constant.
    ''' </summary>
    ''' <value>The constant's imaginary part.</value>
    Public Property ConstantIm As Double
        Get
            Return _constant.ImaginaryPart
        End Get
        Set(ByVal value As Double)
            _constant = New ComplexNumber(_constant.RealPart, value)

            OnPropertyChanged("Constant")
            OnFractalSettingChanged("Constant")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the functio type to use.
    ''' </summary>
    ''' <value>The type.</value>
    Public Property Type As Functions
        Get
            Return _type
        End Get
        Set(ByVal value As Functions)
            _type = value

            OnPropertyChanged("Type")
            OnFractalSettingChanged("Type")
        End Set
    End Property

    ''' <summary>
    ''' Gets the default offset on the viewport.
    ''' </summary>
    ''' <value>The default offset.</value>
    Protected Overrides ReadOnly Property DefaultOffset As ComplexNumber
        Get
            Return 0
        End Get
    End Property

    ''' <summary>
    ''' Gets the default size of the viewport.
    ''' </summary>
    ''' <value>The default size.</value>
    Protected Overrides ReadOnly Property DefaultViewportSize As System.Drawing.SizeF
        Get
            If _type = Functions.Polynomial1 Then
                Return New SizeF(4, 4)
            Else
                Return New SizeF(6.18, 6.18)
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the default escape value.
    ''' </summary>
    ''' <value>The default escape value.</value>
    Protected Overrides ReadOnly Property DefaultBailoutValue As Double
        Get
            If _type = Functions.Polynomial1 Then Return 5

            Return 50
        End Get
    End Property

    ''' <summary>
    ''' Gets the default max iteration count.
    ''' </summary>
    ''' <value>The default max iteration count.</value>
    Protected Overrides ReadOnly Property DefaultMaxIterationCount As Integer
        Get
            Return 50
        End Get
    End Property

    ''' <summary>
    ''' Gets the open CL source.
    ''' </summary>
    ''' <value>The open CL source.</value>
    Protected Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.JuliaKernel
        End Get
    End Property

    ''' <summary>
    ''' Gets the name.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Julia"
        End Get
    End Property

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this fractal is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public Overrides ReadOnly Property RibbonTab As Fluent.RibbonTabItem
        Get
            Dim tab As New RibbonTabItem

            Dim group As New RibbonGroupBox
            group.Header = "Other Settings"
            group.DataContext = Me

            Dim exponentTextBox As New TextBox
            exponentTextBox.Header = "Exponent: "
            exponentTextBox.InputWidth = 50
            exponentTextBox.ToolTip = New ScreenTip With {.Title = "Exponent", .Text = "The Exponent to use in the formula (the k in the funtion)." & Environment.NewLine & "This does not have to be an integral value, so you can also try rational numbers."}
            exponentTextBox.SetBinding(TextBox.TextProperty, "Exponent")
            group.Items.Add(exponentTextBox)

            Dim constantRealTextBox As New TextBox
            constantRealTextBox.Header = "Re(c): "
            constantRealTextBox.InputWidth = 50
            constantRealTextBox.ToolTip = New ScreenTip With {.Title = "Re(c)", .Text = "The real part of the complex parameter c for the displayed julia set."}
            constantRealTextBox.SetBinding(TextBox.TextProperty, "ConstantRe")
            group.Items.Add(constantRealTextBox)

            Dim constantImTextBox As New TextBox
            constantImTextBox.Header = "Im(c): "
            constantImTextBox.InputWidth = 50
            constantImTextBox.ToolTip = New ScreenTip With {.Title = "Im(c)", .Text = "The imaginary part of the complex parameter c for the displayed julia set."}
            constantImTextBox.SetBinding(TextBox.TextProperty, "ConstantIm")
            group.Items.Add(constantImTextBox)

            tab.Groups.Add(group)
            Return tab
        End Get
    End Property

    ''' <summary>
    ''' Gets the description.
    ''' </summary>
    ''' <value>The description.</value>
    Public Overrides ReadOnly Property Description As String
        Get
            Return ""
        End Get
    End Property

    ''' <summary>
    ''' Gets the preview image.
    ''' </summary>
    ''' <value>The image.</value>
    Public Overrides ReadOnly Property PreviewImage As Bitmap
        Get
            Return My.Resources.Julia
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Julia" /> class.
    ''' </summary>
    Public Sub New()
        MyBase.New()

        _exponent = 2
        _constant = New ComplexNumber(-0.6, -0.6)
        _type = Functions.Polynomial1
    End Sub

#End Region

#Region "Loading / Saving"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("Exponent")
        x.WriteValue(_exponent)
        x.WriteEndElement()

        x.WriteStartElement("Constant")
        ComplexNumber.Save(_constant, x)
        x.WriteEndElement()

        x.WriteStartElement("Type")
        x.WriteValue(_type)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        _exponent = CDbl(x.Element("Exponent").Value)
        _constant = ComplexNumber.Load(x.Element("Constant"))
        _type = CType(x.Element("Type").Value, Functions)
    End Sub

#End Region

#Region "Rendering"

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="kernel">The kernel.</param>
    Protected Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel)
        kernel.SetValueArgument(12, _exponent)
        kernel.SetValueArgument(13, New Vectors.Double2(_constant.RealPart, _constant.ImaginaryPart))
    End Sub

#End Region

    Private _exponentArgument As New ValueArgument(Of Double)("exponent", "", 2, "Exponent to use")
    Private _constantArgument As New ValueArgument(Of ComplexNumber)("constant", "", 0, "Value of the complex constant c")

    ''' <summary>
    ''' Gets the arguments.
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property CommandLineArguments As Argument()
        Get
            Return New Argument() {_exponentArgument, _constantArgument}
        End Get
    End Property

    ''' <summary>
    ''' Evaluates the arguments.
    ''' </summary>
    Public Overrides Sub EvaluateCommandLineArguments()
        If _exponentArgument.IsParsed Then
            _exponent = _exponentArgument.Value
        End If

        If _constantArgument.IsParsed Then
            _constant = _constantArgument.Value
        End If
    End Sub
End Class
