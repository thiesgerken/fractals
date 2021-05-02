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
Imports System.Windows.Data
Imports Fluent
Imports Fractals.Mathematics
Imports Fractals.Painters
Imports Fractals.Utilities

''' <summary>
''' Fractal created using Newton's Method for root finding.
''' </summary>
Public Class Newton
    Inherits Fractal
    
#Region "Private Fields"

    Private f As MathFunction(Of ComplexNumber, ComplexNumber)
    Private f1 As MathFunction(Of ComplexNumber, ComplexNumber)
    Private r As ComplexNumber

    Private roots() As ComplexNumber

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the real part of the factor.
    ''' </summary>
    ''' <value>The factor's real part.</value>
    Public Property FactorRe As Double
        Get
            Return r.RealPart
        End Get
        Set(ByVal value As Double)
            r = New ComplexNumber(value, r.ImaginaryPart)

            OnPropertyChanged("Factor")
            OnFractalSettingChanged("Factor")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the imaginary part of the factor.
    ''' </summary>
    ''' <value>The factor's imaginary part.</value>
    Public Property FactorIm As Double
        Get
            Return r.ImaginaryPart
        End Get
        Set(ByVal value As Double)
            r = New ComplexNumber(r.RealPart, value)

            OnPropertyChanged("Factor")
            OnFractalSettingChanged("Factor")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the fz.
    ''' </summary>
    ''' <value>The fz.</value>
    Public Property Fz As String
        Get
            Return f.StringRepresentation
        End Get
        Set(ByVal value As String)
            f = MathFunction(Of ComplexNumber, ComplexNumber).Create(value)

            OnPropertyChanged("Fz")
            OnFractalSettingChanged("Fz")
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets the F1Z.
    ''' </summary>
    ''' <value>The F1Z.</value>
    Public Property F1z As String
        Get
            Return f1.StringRepresentation
        End Get
        Set(ByVal value As String)
            f1 = MathFunction(Of ComplexNumber, ComplexNumber).Create(value)

            OnPropertyChanged("F1z")
            OnFractalSettingChanged("F1z")
        End Set
    End Property

    ''' <summary>
    ''' Gets the default offset on the viewport.
    ''' </summary>
    ''' <value>The default offset.</value>
    Protected Overrides ReadOnly Property DefaultOffset As ComplexNumber
        Get
            Return New ComplexNumber()
        End Get
    End Property

    ''' <summary>
    ''' Gets the default size of the viewport.
    ''' </summary>
    ''' <value>The default size.</value>
    Protected Overrides ReadOnly Property DefaultViewportSize As System.Drawing.SizeF
        Get
            Return New SizeF(4, 4)
        End Get
    End Property

    ''' <summary>
    ''' Gets the default escape value.
    ''' </summary>
    ''' <value>The default escape value.</value>
    Protected Overrides ReadOnly Property DefaultBailoutValue As Double
        Get
            Return 10 ^ -5 'Double.Epsilon
        End Get
    End Property

    ''' <summary>
    ''' Gets the default max iteration count.
    ''' </summary>
    ''' <value>The default max iteration count.</value>
    Protected Overrides ReadOnly Property DefaultMaxIterationCount As Integer
        Get
            Return 45
        End Get
    End Property

    ''' <summary>
    ''' Gets the open CL source.
    ''' </summary>
    ''' <value>The open CL source.</value>
    Protected Overrides ReadOnly Property Source As String
        Get
            Return ""
        End Get
    End Property

    ''' <summary>
    ''' Gets the name.
    ''' </summary>
    ''' <value>The name.</value>
    Public Overrides ReadOnly Property Name As String
        Get
            Return "Newton-Raphson"
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
    ''' Gets a ribbon tab that is displayed in the UI when this fractal is currently active.
    ''' </summary>
    ''' <value>The ribbon tab.</value>
    Public Overrides ReadOnly Property RibbonTab As RibbonTabItem
        Get
            Dim tab As New RibbonTabItem

            Dim group As New RibbonGroupBox
            group.Header = "Other Settings"

            Dim functionTextBox As New TextBox
            functionTextBox.Header = "f(x) ="
            functionTextBox.SetBinding(TextBox.TextProperty, "Fz")
            functionTextBox.InputWidth = 100
            functionTextBox.ToolTip = New ScreenTip With {.Title = "f(x)", .Text = "The Function for which the fractal is calculated."}
            group.Items.Add(functionTextBox)

            Dim derivativeTextBox As New TextBox
            derivativeTextBox.Header = "f'(x) ="
            derivativeTextBox.SetBinding(TextBox.TextProperty, "F1z")
            derivativeTextBox.ToolTip = New ScreenTip With {.Title = "f'(x)", .Text = "The 1. derivative of the function f(x)."}
            derivativeTextBox.InputWidth = 100
            group.Items.Add(derivativeTextBox)

            Dim createButton As New Button
            createButton.Header = "Create Function ...     "
            createButton.SizeDefinition = "Middle"
            createButton.ToolTip = New ScreenTip With {.Title = "Create Function", .Text = "Opens a dialog which will assist you to create a function from given roots."}
            AddHandler createButton.Click, Sub(s, e)
                                               Dim w As New FunctionCreatorWindow
                                               If w.ShowDialog() Then
                                                   Fz = w.Func
                                                   F1z = w.Derivative
                                               End If
                                           End Sub
            group.Items.Add(createButton)

            Dim factorReTextBox As New TextBox
            factorReTextBox.Header = "Re(r) ="
            factorReTextBox.SetBinding(TextBox.TextProperty, New Binding("FactorRe"))
            factorReTextBox.ToolTip = New ScreenTip With {.Title = "Re(r)", .Text = "The Real part of the complex factor r."}
            factorReTextBox.InputWidth = 70
            group.Items.Add(factorReTextBox)

            Dim factorImTextBox As New TextBox
            factorImTextBox.Header = "Im(r) ="
            factorImTextBox.SetBinding(TextBox.TextProperty, New Binding("FactorIm"))
            factorImTextBox.ToolTip = New ScreenTip With {.Title = "Im(r)", .Text = "The Imaginary part of the complex factor r."}
            factorImTextBox.InputWidth = 70
            group.Items.Add(factorImTextBox)

            tab.Groups.Add(group)
            Return tab
        End Get
    End Property

    ''' <summary>
    ''' Gets a list of additional outside painters that can be selected for this fractal even if the painter is not selectable.
    ''' </summary>
    ''' <value>The additional outside painters.</value>
    Public Overrides ReadOnly Property AdditionalOutsidePainters As ICollection(Of System.Type)
        Get
            Return New Type() {GetType(NewtonPainter)}
        End Get
    End Property

    ''' <summary>
    ''' Gets the image.
    ''' </summary>
    ''' <value>The image.</value>
    Public Overrides ReadOnly Property PreviewImage As System.Drawing.Bitmap
        Get
            Return My.Resources.Newton
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Newton" /> class.
    ''' </summary>
    Public Sub New()
        MyBase.New()

        Me.OutsidePainter = New NewtonPainter

        Fz = "x^3-1"
        F1z = "3*x^2"
        r = New ComplexNumber(1, 0)
    End Sub

#End Region

#Region "Methods"

    ' ''' <summary>
    ' ''' Calculates the pixel.
    ' ''' </summary>
    ' ''' <param name="pos">The pos.</param>
    ' ''' <returns></returns>
    'Protected Overrides Function CalculatePixel(ByVal pos As ComplexNumber) As Utilities.IterationResult
    '    Dim mu As Double
    '    Dim n As Integer = 0
    '    Dim z_n As ComplexNumber = pos                       'z_n
    '    Dim z_n1 = z_n + New ComplexNumber(1000, 1000)       'z_(n-1)
    '    Dim temp As ComplexNumber

    '    'Abort when Limit exists and z approaches this limit. It is reached when z_n and z_n+1 do not differ anymore,
    '    'which means their difference is incredibly small

    '    While Not Math.Abs(ComplexNumber.Abs(z_n) - ComplexNumber.Abs(z_n1)) <= _bailoutValue And n < _maxIterations
    '        z_n1 = z_n

    '        temp = f1(z_n)
    '        If temp = 0 Then
    '            z_n = 0
    '        Else
    '            z_n = z_n - r * (f(z_n) / temp)
    '        End If

    '        'Next iteration step
    '        n += 1
    '    End While

    '    If Me.SmoothIterationCounts And n <> _maxIterations Then
    '        mu = Me.Smooth(n, z_n, z_n1)
    '    Else
    '        mu = n
    '    End If

    '    Return New IterationResult(pos, z_n, mu)
    'End Function

    ''' <summary>
    ''' Called before calculation starts, place to (re)initialize private fields used during IterationStep
    ''' </summary>
    Protected Overrides Sub GetReady()
        roots = MathFunctions.GetRoots(f, f1).ToArray

        If OutsidePainter.GetType Is GetType(NewtonPainter) Then
            Dim np As NewtonPainter = CType(OutsidePainter, NewtonPainter)

            np.Roots = roots
        End If
    End Sub

    ''' <summary>
    ''' Smoothes the specified n.
    ''' </summary>
    ''' <param name="n">The n.</param>
    ''' <param name="z">The z.</param>
    ''' <param name="z1">The z1.</param>
    ''' <returns></returns>
    Protected Function Smooth(ByVal n As Integer, ByVal z As ComplexNumber, ByVal z1 As ComplexNumber) As Double
        'Which root was reached?
        Dim root As ComplexNumber
        For Each ro In roots
            If ComplexNumber.AbsSquared(z - ro) <= BailoutValue ^ 2 Then
                root = ro
                Exit For
            End If
        Next

        'compute distances
        Dim lastDistance = ComplexNumber.Abs(z1 - root)
        Dim newDistance = ComplexNumber.Abs(z - root)

        'compute the correction of n
        Dim value As Double = (Math.Log(BailoutValue) - Math.Log(lastDistance)) / (Math.Log(newDistance) - Math.Log(lastDistance))

        'validate value
        If Double.IsNaN(value) Or Double.IsInfinity(value) Then value = 0
        If (n + value) > MaxIterations Then value = 0
        If (n + value) < 0 Then value = 0

        'return n with applied correction
        Return n + value
    End Function


#Region "Loading / Saving"

    ''' <summary>
    ''' Saves the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub SaveParameters(ByVal x As System.Xml.XmlWriter)
        x.WriteStartElement("Function")
        x.WriteValue(Fz)
        x.WriteEndElement()

        x.WriteStartElement("Derivative")
        x.WriteValue(F1z)
        x.WriteEndElement()

        x.WriteStartElement("Factor")
        ComplexNumber.Save(r, x)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads the parameters.
    ''' </summary>
    ''' <param name="x">The x.</param>
    Protected Overrides Sub LoadParameters(ByVal x As System.Xml.Linq.XElement)
        Fz = x.Element("Function").Value
        F1z = x.Element("Derivative").Value

        r = ComplexNumber.Load(x.Element("Factor"))
    End Sub

#End Region
#End Region
End Class
