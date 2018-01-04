if exists (select * from sys.objects where name = 'Find_Text_In_SP' )
    drop PROCEDURE [dbo].[Find_Text_In_SP]     
	Go

CREATE PROCEDURE [dbo].[Find_Text_In_SP]          
@StringToSearch varchar(100)           
AS        
   SET @StringToSearch = '%' +@StringToSearch + '%'          
   SELECT Distinct SO.Name ,        
	 SO.Type,     
   CASE SO.TYPE     
   WHEN 'P' THEN 'Store Proc'        
   WHEN 'FN' THEN 'Function'        
   WHEN 'V' THEN 'View'        
   WHEN 'TR' THEN 'Trigger'  
   WHEN 'U' THEN 'Table'      
   ELSE SO.Type        
   END AS 'Type'         
   FROM sysobjects SO (NOLOCK)          
	 left  JOIN syscomments SC (NOLOCK) on SO.Id = SC.ID          
   where (SC.Text LIKE @stringtosearch  or    SO.Name      LIKE @stringtosearch)
   and SO.Type not in ('IT','K','F')
   ORDER BY SO.Type, SO.Name   
GO
if exists (select * from sys.objects where name = 'Find_Text_In_Column' )
    drop PROCEDURE [dbo].Find_Text_In_Column     
	Go

CREATE PROCEDURE [dbo].[Find_Text_In_Column] @StringToSearch varchar(100)
AS
BEGIN
  SET @StringToSearch = '%' + @StringToSearch + '%'
  SELECT
    a.Name,
    a.[Type],
    a.TypeName
  FROM (SELECT
    scl.Name AS Name,
    so.Name AS Type,
    'Column' AS 'TypeName'
  FROM sys.columns scl (NOLOCK)
  JOIN sys.objects SO (NOLOCK)
    ON scl.object_id = SO.object_id
  LEFT JOIN syscomments SC (NOLOCK)
    ON SO.object_id = SC.ID
  WHERE (scl.Name LIKE @stringtosearch)
  --ORDER BY  SO.Name ,scl.Name  
  UNION
  SELECT DISTINCT
    SO.Name,
    SO.Type,
    CASE SO.TYPE
      WHEN 'P' THEN 'Store Proc'
      WHEN 'FN' THEN 'Function'
      WHEN 'V' THEN 'View'
      WHEN 'TR' THEN 'Trigger'
      WHEN 'U' THEN 'Table'
      ELSE SO.Type
    END AS 'TypeName'
  FROM sysobjects SO (NOLOCK)
  LEFT JOIN syscomments SC (NOLOCK)
    ON SO.Id = SC.ID
  WHERE (SC.Text LIKE @stringtosearch
  OR SO.Name LIKE @stringtosearch)
  AND SO.Type NOT IN ('IT', 'K')) AS a

  ORDER BY a.[TypeName], a.Name

END
GO

if exists (select * from sys.objects where name = 'select_top_100_query' )
    drop PROCEDURE [dbo].select_top_100_query     
	Go
CREATE PROCEDURE [dbo].select_top_100_query          
@StringToSearch varchar(100)           
AS       
begin 
 declare @sql nvarchar(max)
 DECLARE @keys NVARCHAR(100) 
 SET @keys= ''
SELECT @keys =  @keys+ CASE WHEN @keys ='' THEN '' ELSE ', ' END + 'a.' +  b.name + ' DESC'                          
 FROM   sys.objects AS a                  
        INNER JOIN sys.columns AS b                  
             ON  a.object_id = b.object_id                  
        JOIN sys.types t                  
             ON  b.system_type_id = t.system_type_id                  
        LEFT   JOIN sys.indexes AS i                  
             ON  i.OBJECT_ID = b.OBJECT_ID                  
             AND i.is_primary_key = 1                  
        LEFT  JOIN sys.index_columns AS ic                  
             ON  i.OBJECT_ID = ic.OBJECT_ID                  
             AND i.index_id = ic.index_id                  
             AND ic.column_id = b.column_id                  
 WHERE  a.name = @StringToSearch     AND t.[name] <> 'timestamp' AND t.[name] <> 'sysname'     AND b.is_computed = 0    
 AND ic.[object_id] IS NOT NULL 

 set @sql = 'select top(100) * from ' + @StringToSearch  + ' a' 
 IF @keys <> '' 
 BEGIN
    SET @sql = @sql + ' ORDER BY ' + @keys
  END
  --exec sp_executesql @sql
  select @sql
END


GO

if exists (select * from sys.objects where name = 'select_top_100' )
    drop PROCEDURE [dbo].select_top_100     
	Go
CREATE PROCEDURE [dbo].select_top_100          
@StringToSearch varchar(100)           
AS       
begin 
 declare @sql nvarchar(max)
 DECLARE @keys NVARCHAR(100) 
 SET @keys= ''
 SELECT @keys =  @keys+ CASE WHEN @keys ='' THEN '' ELSE ', ' END + 'a.' +  b.name + ' DESC'                         
 FROM   sys.objects AS a                  
        INNER JOIN sys.columns AS b                  
             ON  a.object_id = b.object_id                  
        JOIN sys.types t                  
             ON  b.system_type_id = t.system_type_id                  
        LEFT   JOIN sys.indexes AS i                  
             ON  i.OBJECT_ID = b.OBJECT_ID                  
             AND i.is_primary_key = 1                  
        LEFT  JOIN sys.index_columns AS ic                  
             ON  i.OBJECT_ID = ic.OBJECT_ID                  
             AND i.index_id = ic.index_id                  
             AND ic.column_id = b.column_id                  
 WHERE  a.name = @StringToSearch     AND t.[name] <> 'timestamp' AND t.[name] <> 'sysname'     AND b.is_computed = 0    
 AND ic.[object_id] IS NOT NULL 

 set @sql = 'select top(100) * from ' + @StringToSearch + ' a' 
 IF @keys <> '' 
 BEGIN
    SET @sql = @sql + ' ORDER BY ' + @keys
  END
  exec sp_executesql @sql
  --select @sql
END


GO

if exists (select * from sys.objects where name = 'st_generate_datadump_tablename' )
    drop PROCEDURE [dbo].st_generate_datadump_tablename     
	Go
CREATE  PROCEDURE st_generate_datadump_tablename                  
 @table_name VARCHAR(100)                  
AS                  
BEGIN                  
 DECLARE @tbl TABLE                   
         (                  
             id INT IDENTITY(1, 1),                  
             tableName NVARCHAR(200),                  
             columnName NVARCHAR(200),                  
             columnType VARCHAR(50),                  
             max_length INT,                  
             is_identity BIT,                  
             IsKey BIT                  
         )                          
                   
 INSERT INTO @tbl                  
   (                  
     tableName,                  
     columnName,                  
     columnType,                  
     max_length,                  
     IsKey,                  
     is_identity                  
   )                  
 SELECT a.name AS tableName,                  
        b.name AS columnName,                  
        t.name,                  
        b.max_length,                  
        CASE                   
             WHEN ic.[object_id] IS NULL THEN 0                  
             ELSE 1                  
        END AS IsKey,                  
        b.is_identity                  
 FROM   sys.objects AS a                  
        INNER JOIN sys.columns AS b                  
             ON  a.object_id = b.object_id                  
        JOIN sys.types t                  
             ON  b.system_type_id = t.system_type_id                  
        LEFT    JOIN sys.indexes AS i                  
             ON  i.OBJECT_ID = b.OBJECT_ID                  
             AND i.is_primary_key = 1                  
        LEFT  JOIN sys.index_columns AS ic                  
             ON  i.OBJECT_ID = ic.OBJECT_ID                  
             AND i.index_id = ic.index_id                  
             AND ic.column_id = b.column_id                  
 WHERE  a.name = @table_name     AND t.[name] <> 'timestamp' AND t.[name] <> 'sysname'     AND b.is_computed = 0      
 ORDER BY                  
        a.name                      
                   
                   
 DECLARE @IsIdentity BIT                  
 SET @IsIdentity = 0                      
                   
 IF EXISTS(                  
        SELECT 1                  
        FROM   @tbl                  
        WHERE  is_identity = 1                  
    )                  
     SET @IsIdentity = 1                         
                   
 DECLARE @sql NVARCHAR(MAX)                     
 SET @sql = ''                       
 SET @sql =     @sql +              
     ' DECLARE @tbl_temp TABLE(Id int  IDENTITY(1,1),ResultText VARCHAR(MAX))  '            
       SET @sql = @sql + ' INSERT INTO @tbl_temp(ResultText) SELECT ''GO'' '                               
 IF @IsIdentity = 1                  
     SET @sql = @sql + ' INSERT INTO @tbl_temp(ResultText) SELECT ''SET IDENTITY_INSERT ' + @table_name + ' ON'' '                      
                   
 SET @sql = @sql + ' INSERT INTO @tbl_temp(ResultText)  SELECT ''' +                   
     ' INSERT INTO ' + @table_name + ' ('                          
                   
 SELECT @sql = @sql + '[' + t.columnName + '],'                  
 FROM   @tbl t                            
                   
 SET @sql = SUBSTRING(@sql, 1, LEN(@sql) - 1)                          
                   
                   
                   
 SET @sql = @sql + ' ) VALUES ( '' + '                          
                   
 SELECT @sql = @sql + 'CASE WHEN [' + t.columnName + '] IS NULL THEN ''NULL'' ELSE '                   
        + CASE                   
               WHEN t.columnType LIKE '%char%' THEN ''''''''' + ' +                   
                    'REPLACE( REPLACE([' + t.columnName + '],'''''''',''''''''''''), CHAR(13)+CHAR(10),'''''' + CHAR(13)+CHAR(10) + '''''') '                   
                    + ' + ''''''''' + ' END '                  
           WHEN t.columnType LIKE '%TEXT%' OR t.columnType LIKE '%UNIQUEIDENTIFIER%'  THEN ''''''''' + ' +                   
                    ' REPLACE(REPLACE(CAST([' + t.columnName + '] AS VARCHAR(MAX)),'''''''',''''''''''''), CHAR(13)+CHAR(10),'''''' + CHAR(13)+CHAR(10) + '''''')'                   
                    + ' + ''''''''' + ' END '                  
               WHEN t.columnType LIKE '%DATE%' THEN '''CONVERT(DATETIME,'''''' + ' + ' CONVERT(VARCHAR(30),' + t.columnName + ',131)'                   
                    + ' + '''''',131)''' + ' END '                  
           ELSE '  CAST([' + t.columnName + '] AS VARCHAR(100)) END '                  
          END + ' + '','' + '                  
 FROM   @tbl t                             
                   
 SET @sql = SUBSTRING(@sql, 1, LEN(@sql) - 4)                          
 SET @sql = @sql + ' )'' FROM ' + @table_name                          
                   
 IF @IsIdentity = 1                  
     SET @sql = @sql + ' INSERT INTO @tbl_temp(ResultText) SELECT ''SET IDENTITY_INSERT ' + @table_name + ' OFF'' '                      
                   
 SET @sql = @sql + ' SELECT ResultText FROM  @tbl_temp Order By Id '                   
  -- PRINT @sql                
 EXEC sp_executesql @sql                  
END 
