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

Imports System.Windows.Data
Imports System.Globalization
Imports System.Windows.Shell
Imports System.Windows.Media
Imports System.Windows
Imports System.Windows.Controls
Imports Fluent
Imports Fractals.Utilities
Imports Fractals.Mathematics
Imports Brush = System.Windows.Media.Brush
Imports Brushes = System.Windows.Media.Brushes

''' <summary>
''' Implements a TimeSpan -> String OneWay Converter creating output like '123ms'
''' </summary>
<ValueConversion(GetType(TimeSpan), GetType(String))>
Public Class TimeConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val = CType(value, TimeSpan).TotalMilliseconds

        If Not parameter Is Nothing Then
            If GetType(String) = parameter.GetType Then
                If CStr(parameter) = "Minus" Then val *= -1
            End If
        End If

        Return Math.Round(val, 0) & " ms"
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class

''' <summary>
''' Implements a Double -> String OneWay Converter creating output like '237,123 kHz'
''' </summary>
<ValueConversion(GetType(Double), GetType(String))>
Public Class SpeedConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val As Double = CType(value, Double)

        If val > 1000000 Then
            Return (val / 1000000).ToString("##0.00") & " MHz"
        Else
            Return (val / 1000).ToString("##0.00") & " kHz"
        End If
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class

''' <summary>
''' Implements a Double -> String and WorkingState -> ProgressState OneWay Converter creating output like '10.23%' or ProgressState.Normal
''' </summary>
Public Class ProgressConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        If targetType Is GetType(String) Then
            Dim val As Double = CType(value, Double)
            If val < 0 Then val = 0
            If val > 1 Then val = 1

            Return Math.Round(val * 100, 2).ToString("##0.00") & "%"
        ElseIf targetType Is GetType(TaskbarItemProgressState) Then
            Dim val As WorkingState = CType(value, WorkingState)

            Select Case val
                Case WorkingState.Working
                    Return TaskbarItemProgressState.Normal
                Case WorkingState.Paused
                    Return TaskbarItemProgressState.Paused
                Case Else
                    Return TaskbarItemProgressState.None
            End Select
        ElseIf targetType Is GetType(Brush) Then
            Dim val As WorkingState = CType(value, WorkingState)

            If val = WorkingState.Paused Then
                Return Brushes.Yellow
            Else
                Return New SolidColorBrush(System.Windows.Media.Color.FromArgb(255, 1, 211, 40))
            End If
        Else
            Return 0
        End If
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class

''' <summary>
''' Implements a Double -> String OneWay Converter creating output like '32,12 MiB'
''' </summary>
<ValueConversion(GetType(Double), GetType(String))>
Public Class MemorySizeConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val As Double = CType(value, Double)
        Return Math.Round(val / 1024 / 1024, 0).ToString & " MiB"
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class


''' <summary>
''' Implements a Size -> String OneWay Converter creating output like '100 x 200'
''' </summary>
<ValueConversion(GetType(System.Drawing.Size), GetType(String))>
Public Class SizeConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val As System.Drawing.Size = CType(value, System.Drawing.Size)

        Return val.Width & " x " & val.Height & " px"
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class

''' <summary>
''' Implements a Double -> Thickness OneWay Converter to align a control under another one (see GUI for an example) 
''' </summary>
<ValueConversion(GetType(Double), GetType(Thickness))>
Public Class MarginConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val As Double = CType(value, Double)

        Return New Thickness(0, val, 0, 0)
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class

''' <summary>
''' Implements a ZoomState/String Converter for individual components of the zoom state.
''' </summary>
<ValueConversion(GetType(ZoomState), GetType(Double))>
Public Class ZoomStateConverter
    Implements IValueConverter

    Private _state As ZoomState

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Try
            Dim val As ZoomState = CType(value, ZoomState)
            Dim para As String = CStr(parameter)

            _state = val

            Select Case para.ToLowerInvariant
                Case "factor"
                    Return val.Factor
                Case "offsetre"
                    Return val.Offset.RealPart
                Case "offsetim"
                    Return val.Offset.ImaginaryPart
                Case "rotation"
                    Return val.Rotation / Math.PI * 180
            End Select

            Return 0.0R
        Catch ex As Exception
            Return 0.0R
        End Try
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Try
            Dim val As Double = CType(value, Double)
            Dim para As String = CStr(parameter)

            Select Case para.ToLowerInvariant
                Case "factor"
                    _state.Factor = val
                Case "offsetre"
                    _state.Offset = New ComplexNumber(val, _state.Offset.ImaginaryPart)
                Case "offsetim"
                    _state.Offset = New ComplexNumber(_state.Offset.RealPart, val)
                Case "rotation"
                    _state.Rotation = val * Math.PI / 180
            End Select

            Return _state
        Catch ex As Exception
            Return _state
        End Try
    End Function
End Class

''' <summary>
''' Implements a Boolean/Boolean Converter that returns the opposite value.
''' </summary>
<ValueConversion(GetType(Boolean), GetType(Boolean))>
Public Class NotConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Try
            Dim val = CType(value, Boolean)

            Return Not val
        Catch ex As Exception
            Return False
        End Try
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Return Convert(value, targetType, parameter, culture)
    End Function
End Class

''' <summary>
''' Implements a Integer/String Converter for AntiAliasing Sample counts, generates Output like '2x2'
''' </summary>
<ValueConversion(GetType(Integer), GetType(String))>
Public Class SamplesConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val = Math.Sqrt(CInt(value))

        Return val & " x " & val
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Try
            Dim str = CStr(value).Replace(" "c, "")

            If Not str.Contains("x") Then
                'only one number

                Dim int = CInt(str)
                Dim root = Math.Sqrt(int)

                If root <> CInt(root) Then Throw New ArgumentException("Samples have to be squares")

                Return int
            Else
                Dim splits() = str.Split("x"c)

                Dim val1 As Integer = CInt(splits(0))
                Dim val2 As Integer = CInt(splits(1))

                If val1 <> val2 Then Throw New ArgumentException("Samples have to be squares")

                Return val1 * val2
            End If
        Catch ex As Exception
            Return 1
        End Try
    End Function
End Class

''' <summary>
''' Implements a Converter that can be used to bind to methods.
''' </summary>
Public Class MethodToValueConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim methodName = CStr(parameter)
        If value Is Nothing Or methodName = Nothing Then Return value

        Dim methodInfo = value.GetType().GetMethod(methodName, Type.EmptyTypes)
        If methodInfo Is Nothing Then Return value

        Return methodInfo.Invoke(value, New Object() {})
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class

''' <summary>
''' Implements a Converter that converts a filename to a window caption.
''' </summary>
<ValueConversion(GetType(String), GetType(String))>
Public Class FilenameWindowTitleConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val As String = CStr(value)

        If val = "" Then
            val = "Untitled"
        Else
            val = IO.Path.GetFileName(val)
        End If
        
        Return "Fractals - " & val
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Throw New NotSupportedException("Cannot convert back")
    End Function
End Class


''' <summary>
''' Implements a Color/SolidColorBrush Converter 
''' </summary>
<ValueConversion(GetType(Color), GetType(SolidColorBrush))>
Public Class ColorBrushConverter
    Implements IValueConverter

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value produced by the binding source.</param>
    ''' <param name="targetType">The type of the binding target property.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function Convert(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.Convert
        Dim val As Color = CType(value, Color)

        Return New SolidColorBrush(val)
    End Function

    ''' <summary>
    ''' Converts a value.
    ''' </summary>
    ''' <param name="value">The value that is produced by the binding target.</param>
    ''' <param name="targetType">The type to convert to.</param>
    ''' <param name="parameter">The converter parameter to use.</param>
    ''' <param name="culture">The culture to use in the converter.</param>
    ''' <returns>
    ''' A converted value. If the method returns null, the valid null value is used.
    ''' </returns>
    Public Function ConvertBack(ByVal value As Object, ByVal targetType As Type, ByVal parameter As Object, ByVal culture As CultureInfo) As Object Implements IValueConverter.ConvertBack
        Dim val As SolidColorBrush = CType(value, SolidColorBrush)

        Return val.Color
    End Function
End Class
