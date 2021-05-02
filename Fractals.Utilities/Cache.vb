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

Imports System.Runtime.InteropServices
Imports System.IO
Imports System.Windows.Forms
Imports ICSharpCode.SharpZipLib.BZip2

''' <summary>
''' Class able to cache data to both memory and filesystem, as long as the type to cache implements ICacheable.
''' </summary>
''' <typeparam name="T">the classtype to cache</typeparam>
Public Class Cache(Of T As {IBinarySaveable, New})

#Region "Fields"

    Private _mode As CachingMode
    Private _data() As T
    Private _hashString As String
    Private _hashBytes() As Byte
    Private Shared _cacheOrder As New Queue(Of String)
    Private Shared _previousData As New Dictionary(Of String, T())

#End Region

#Region "Properties"

    ''' <summary>
    ''' Gets or sets the maximum size of the memory cache.
    ''' </summary>
    ''' <value>The maximum size of the memory cache in bytes.</value>
    Public Shared Property MaxMemCacheSize As Long = 1L * 1024 * 1024 * 1024 '1GB, not so much on modern computers

    ''' <summary>
    ''' Gets or sets the maximum size of the file cache.
    ''' </summary>
    ''' <value>The maximum size of the file cache in bytes.</value>
    Public Shared Property MaxFileCacheSize As Long = 4L * 1024 * 1024 * 1024 '10GB

    ''' <summary>
    ''' Gets or sets the maximum file cache age.
    ''' </summary>
    ''' <value>The maximum file cache age in minutes.</value>
    Public Shared Property MaxFileCacheAge As Integer = 7 * 24 * 60 '1 Week

    ''' <summary>
    ''' Gets or sets the data of the cache at a given position.
    ''' </summary>
    ''' <value>The data.</value>
    Default Public Property Data(ByVal i As Integer) As T
        Get
            Return _data(i)
        End Get
        Set(ByVal value As T)
            If Not _mode = CachingMode.Write Then Throw New Exception("Cache is sealed")
            _data(i) = value
        End Set
    End Property

    ''' <summary>
    ''' Gets or sets a value indicating whether to zip the cache files.
    ''' </summary>
    ''' <value><c>true</c> if files should get zipped; otherwise, <c>false</c>.</value>
    Public Property ZipFiles As Boolean = False

    ''' <summary>
    ''' Gets the hash that identifies this cache.
    ''' </summary>
    ''' <value>The hash.</value>
    Public ReadOnly Property Hash As String
        Get
            Return _hashString
        End Get
    End Property

    ''' <summary>
    ''' Gets the mode of the cache
    ''' </summary>
    ''' <value>The current mode.</value>
    Public ReadOnly Property Mode As CachingMode
        Get
            Return _mode
        End Get
    End Property

    ''' <summary>
    ''' Gets the size of the mem cache in bytes.
    ''' </summary>
    ''' <value>The size of the mem cache in bytes.</value>
    Public Shared ReadOnly Property MemCacheSize As Long
        Get
            Dim sizePerItem As Long = Marshal.SizeOf(New T)
            Dim size As Long = 0

            For Each kvp In _previousData
                size += kvp.Value.Length * sizePerItem
            Next

            Return size
        End Get
    End Property

    ''' <summary>
    ''' Gets the size of the file cache in bytes.
    ''' </summary>
    ''' <value>The size of the file cache in bytes.</value>
    Public Shared ReadOnly Property FileCacheSize As Long
        Get
            If Not Directory.Exists(CacheFolder) Then Return 0

            Dim size As Long = 0

            For Each f In Directory.GetFiles(CacheFolder, "*.cache", SearchOption.AllDirectories)
                size += New FileInfo(f).Length
            Next

            Return size
        End Get
    End Property

    ''' <summary>
    ''' Gets or sets the cache folder.
    ''' </summary>
    ''' <value>The cache folder.</value>
    Private Shared ReadOnly Property CacheFolder As String
        Get
            Return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), Application.ProductName & "\Cache\")
        End Get
    End Property

    ''' <summary>
    ''' Gets or sets the cache path.
    ''' </summary>
    ''' <value>The cache path.</value>
    Private ReadOnly Property CachePath As String
        Get
            Return CacheFolder & _hashString & ".cache"
        End Get
    End Property

#End Region

#Region "Constructor"

    ''' <summary>
    ''' Initializes a new instance of the <see cref="Cache(Of T)" /> class.
    ''' </summary>
    ''' <param name="hash">The hash.</param>
    ''' <param name="size">The size when created new.</param>
    Public Sub New(ByVal hash As Byte(), ByVal size As Integer)
        _hashBytes = hash
        _hashString = HashToString(hash)

        'try reading from cachecache
        If _previousData.ContainsKey(_hashString) Then
            Trace.WriteLine("Data is memcached")
            _data = _previousData(_hashString)
            _mode = CachingMode.Read
        Else
            'try reading from file
            Dim success = False
            ReDim _data(size - 1)

            Try
                If File.Exists(CachePath) Then
                    'Try reading from cache
                    Using st As New FileStream(CachePath, FileMode.Open)
                        Dim zipped As Boolean = CBool(st.ReadByte)

                        Using ms As New MemoryStream
                            If zipped Then
                                BZip2.Decompress(st, ms, False)
                            Else
                                st.CopyTo(ms)
                            End If

                            ms.Position = 0

                            Using reader As New BinaryReader(ms)
                                'double-check hash code (file names are modified easily)
                                Dim cacheHash = reader.ReadBytes(16)
                                Dim cacheHashstr = HashToString(cacheHash)

                                If cacheHashstr = _hashString Then
                                    Trace.WriteLine("Loading Cache from file ...")

                                    For i As Integer = 0 To _data.Length - 1
                                        Dim value As New T
                                        value.Load(reader)

                                        _data(i) = value
                                    Next

                                    'save in cachecache
                                    _previousData.Add(_hashString, _data)
                                    _cacheOrder.Enqueue(_hashString)

                                    success = True
                                End If

                                reader.Close()
                            End Using
                        End Using
                    End Using
                End If
            Catch ex As Exception
            End Try

            If Not success Then
                _mode = CachingMode.Write
            Else
                _mode = CachingMode.Read
            End If
        End If
    End Sub

#End Region

#Region "Methods"

    ''' <summary>
    ''' Returns a String Representation of a hash
    ''' </summary>
    ''' <param name="hash">The hash.</param>
    ''' <returns></returns>
    Private Shared Function HashToString(ByVal hash() As Byte) As String
        Dim hashstr = ""

        For Each b In hash
            hashstr &= BitConverter.ToString(New Byte() {b}) '16 Bytes
        Next

        Return hashstr
    End Function

    ''' <summary>
    ''' Saves the cache in both memory and filesystem.
    ''' </summary>
    Public Sub Cache()
        If _mode = CachingMode.Read Then Return 'already sealed?

        'go into reading mode
        _mode = CachingMode.None

        'save in cachecache
        _previousData.Add(_hashString, _data)
        _cacheOrder.Enqueue(_hashString)

        'save to file
        If Not Directory.Exists(CacheFolder) Then
            Directory.CreateDirectory(CacheFolder)
        End If

        If File.Exists(CachePath) Then File.Delete(CachePath)
        Using st As New FileStream(CachePath, FileMode.Create)
            st.WriteByte(CByte(ZipFiles))

            Using ms As New MemoryStream
                Using writer As New BinaryWriter(ms)
                    writer.Write(_hashBytes)

                    For i As Integer = 0 To _previousData(_hashString).Length - 1
                        _data(i).Save(writer)
                    Next

                    ms.Seek(0, SeekOrigin.Begin)

                    If ZipFiles Then
                        BZip2.Compress(ms, st, True, 4026)
                    Else
                        ms.CopyTo(st)
                    End If
                End Using
            End Using
        End Using

        Trace.WriteLine("Caching Finished")

        CheckMemoryConsumption()
        CheckHardDiskConsumption()
    End Sub

    ''' <summary>
    ''' Checks if it is needed to free memory.
    ''' </summary>
    Private Sub CheckMemoryConsumption()
        While MemCacheSize > MaxMemCacheSize And _previousData.Count > 0
            _previousData.Remove(_cacheOrder.Dequeue) 'Delete first item that was cached (lowest chance that this dataset is needed again)

            Trace.WriteLine("Cleaned Memory Cache because it exceeded the maximum size")
        End While
    End Sub

    ''' <summary>
    ''' Checks if it is needed to free disk space.
    ''' </summary>
    Private Sub CheckHardDiskConsumption()
        'first check file age
        For Each f In Directory.GetFiles(CacheFolder, "*.cache", SearchOption.AllDirectories)
            If (Date.Now - New FileInfo(f).LastWriteTime).TotalMinutes > MaxFileCacheAge Then File.Delete(f)
        Next

        'more freeing needed?
        If FileCacheSize > MaxFileCacheSize Then
            While FileCacheSize > MaxFileCacheSize
                Dim files As New List(Of KeyValuePair(Of String, Integer))
                For Each f In Directory.GetFiles(CacheFolder, "*.cache", SearchOption.AllDirectories)
                    files.Add(New KeyValuePair(Of String, Integer)(f, CInt((Date.Now - New FileInfo(f).LastWriteTime).TotalMinutes)))
                Next

                files.Sort(Function(first, second) first.Value.CompareTo(second.Value) * -1)

                File.Delete(files(0).Key) 'Delete oldest file
            End While

            Trace.WriteLine("Cleaned File Cache because it exceeded the maximum size")
        End If
    End Sub

#End Region
End Class
