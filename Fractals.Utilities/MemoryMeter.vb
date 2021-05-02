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

Imports System.ComponentModel
Imports System.Threading

''' <summary>
''' Meter for Cache Sizes
''' </summary>
Public Class MemoryMeter(Of T As {New, IBinarySaveable})
    Implements INotifyPropertyChanged, IDisposable

    Private _updater As Thread

    ''' <summary>
    ''' Gets the amount of memory used by the current Process.
    ''' </summary>
    ''' <value>The memory usage.</value>
    Public ReadOnly Property MemoryUsage As Long
        Get
            Return Process.GetCurrentProcess.WorkingSet64
        End Get
    End Property

    ''' <summary>
    ''' Gets the hard drive usage of the cache (of T) in bytes.
    ''' </summary>
    ''' <value>The hard drive usage.</value>
    Public ReadOnly Property CacheHardDriveUsage As Long
        Get
            Return Cache(Of T).FileCacheSize
        End Get
    End Property

    ''' <summary>
    ''' Initializes a new instance of the <see cref="MemoryMeter" /> class.
    ''' </summary>
    Public Sub New()
        _updater = New Thread(Sub()
                                  Try
                                      While True
                                          OnPropertyChanged("CacheHardDriveUsage")
                                          OnPropertyChanged("MemoryUsage")

                                          For i As Integer = 0 To 9
                                              Thread.Sleep(500)
                                              If disposedValue Then Return
                                          Next
                                      End While
                                  Catch ex As ThreadAbortException
                                  End Try
                              End Sub)

        _updater.Start()
    End Sub
    
    ''' <summary>
    ''' Called when [property changed].
    ''' </summary>
    ''' <param name="propertyName">Name of the property.</param>
    Private Sub OnPropertyChanged(ByVal propertyName As String)
        RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
    End Sub

    ''' <summary>
    ''' Occurs when a property value changes.
    ''' </summary>
    Public Event PropertyChanged(ByVal sender As Object, ByVal e As PropertyChangedEventArgs) Implements INotifyPropertyChanged.PropertyChanged

#Region "IDisposable Support"
    Private disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Overridable Sub Dispose(ByVal disposing As Boolean)
        If Not Me.disposedValue Then
            If disposing Then
                'dispose managed state (managed objects).
            End If
        End If
        Me.disposedValue = True
    End Sub

    ' This code added by Visual Basic to correctly implement the disposable pattern.
    Public Sub Dispose() Implements IDisposable.Dispose
        ' Do not change this code.  Put cleanup code in Dispose(ByVal disposing As Boolean) above.
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub
#End Region

End Class
