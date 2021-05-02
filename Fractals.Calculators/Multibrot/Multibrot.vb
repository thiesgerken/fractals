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
Imports System.Runtime.InteropServices
Imports System.Drawing.Imaging
Imports Cloo
Imports CommandLineParser
Imports Fluent
Imports Fractals.Mathematics
Imports Fractals.Utilities
Imports System.Threading
Imports System.Threading.Tasks
Imports System.Text

''' <summary>
''' Displays Mandelbrot- and Mandelbrot related sets.
''' </summary>
Public Class Multibrot
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
        Polynomial
        ''' <summary>
        ''' z = e^(|Re(z)| + |Im(z)|i) * c
        ''' </summary>
        Exponential
        ''' <summary>
        ''' z = conjugate(z) ^ k + c (also known as 'Tricorn')
        ''' </summary>
        Conjugate
        ''' <summary>
        ''' z = (|Re(z)+|Im(z)|) ^ k + c (also known as 'Burningship')
        ''' </summary>
        Abs
    End Enum
#End Region

#Region "Private Fields"

    Private _exponent As Double
    Private _invert As Boolean
    Private _type As Functions

#End Region

#Region "Properties"

    Private _exponentArgument As New ValueArgument(Of Double)("exponent", "", 2, "Exponent to use")
    Private _invertArgument As New SwitchArgument("invert", "", False, "Use 1/c instead of c")

    ''' <summary>
    ''' Gets the arguments.
    ''' </summary>
    ''' <value>The arguments.</value>
    Public Overrides ReadOnly Property CommandLineArguments As Argument()
        Get
            Return New Argument() {_exponentArgument, _invertArgument}
        End Get
    End Property

    ''' <summary>
    ''' Evaluates the arguments.
    ''' </summary>
    Public Overrides Sub EvaluateCommandLineArguments()
        If _exponentArgument.IsParsed Then
            _exponent = _exponentArgument.Value
        End If

        If _invertArgument.IsParsed Then
            _invert = True
        End If
    End Sub

    ''' <summary>
    ''' Gets or sets the exponent.
    ''' </summary>
    ''' <value>The exponent.</value>
    Public Property Exponent As Double
        Get
            Return _exponent
        End Get
        Set(ByVal value As Double)
            If value <= 0 Then Throw New Exception("The Exponent has to be a positive number.")

            _exponent = value

            OnPropertyChanged("Exponent")
            OnFractalSettingChanged("Exponent")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether this <see cref="Multibrot" /> is inverted.
    ''' </summary>
    ''' <value><c>true</c> if invert; otherwise, <c>false</c>.</value>
    Public Property Invert As Boolean
        Get
            Return _invert
        End Get
        Set(ByVal value As Boolean)
            _invert = value

            OnPropertyChanged("Invert")
            OnFractalSettingChanged("Invert")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the type.
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
            If _invert And _exponent = 2 Then
                Return 1.3
            ElseIf _exponent = 2 Then
                Return -0.6
            Else
                Return 0
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the default size of the viewport.
    ''' </summary>
    ''' <value>The default size.</value>
    Protected Overrides ReadOnly Property DefaultViewportSize As System.Drawing.SizeF
        Get
            If _invert Then
                Return New SizeF(5.4, 5.4)
            Else
                Return New SizeF(2.6, 2.6)
            End If
        End Get
    End Property

    ''' <summary>
    ''' Gets the default escape value.
    ''' </summary>
    ''' <value>The default escape value.</value>
    Protected Overrides ReadOnly Property DefaultBailoutValue As Double
        Get
            Return 10
        End Get
    End Property

    ''' <summary>
    ''' Gets the default max iteration count.
    ''' </summary>
    ''' <value>The default max iteration count.</value>
    Protected Overrides ReadOnly Property DefaultMaxIterationCount As Integer
        Get
            Return 100
        End Get
    End Property

    ''' <summary>
    ''' Gets the open CL source.
    ''' </summary>
    ''' <value>The open CL source.</value>
    Protected Overrides ReadOnly Property Source As String
        Get
            Return My.Resources.MultibrotKernel
        End Get
    End Property

    ''' <summary>
    ''' Gets a ribbon tab that is displayed in the UI when this fractal is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public Overrides ReadOnly Property RibbonTab As RibbonTabItem
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

            Dim invertCheckBox As New CheckBox
            invertCheckBox.Header = "Invert"
            invertCheckBox.ToolTip = New ScreenTip With {.Title = "Invert", .Text = "Value determining whether to invert the fractal (use 1/c instead of c)"}
            invertCheckBox.SetBinding(CheckBox.IsCheckedProperty, "Invert")
            group.Items.Add(invertCheckBox)

            tab.Groups.Add(group)
            Return tab
        End Get
    End Property

    ''' <summary>
    ''' Gets the name.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Multibrot"
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
    ''' Gets the image.
    ''' </summary>
    ''' <value>The image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.Multibrot
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Multibrot" /> class.
    ''' </summary>
    Public Sub New()
        MyBase.New()

        _exponent = 2
        _invert = False
        _type = Functions.Polynomial
    End Sub

#End Region

#Region "Rendering"

    ''' <summary>
    ''' Sets the arguments.
    ''' </summary>
    ''' <param name="kernel">The kernel.</param>
    Protected Overrides Sub SetArguments(ByVal kernel As Cloo.ComputeKernel)
        kernel.SetValueArgument(12, _exponent)
        kernel.SetValueArgument(13, If(_invert, 1, 0))
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

        x.WriteStartElement("Invert")
        x.WriteValue(_invert)
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
        _invert = CBool(x.Element("Invert").Value)
        _type = CType(x.Element("Type").Value, Functions)
    End Sub
#End Region
End Class
