USE QL;

SET NOCOUNT ON;

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- CREATE TABLE for @dim dimension
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CREATE OR ALTER PROCEDURE dbo.CREATE_matrix
	
	@dim INT

AS
BEGIN
	DECLARE @i INT = 1
	DECLARE @j INT = 1

	DECLARE @TableCreate NVARCHAR(2000) = 
	'DROP TABLE IF EXISTS dbo.T_2048; 
	CREATE TABLE dbo.T_2048 (ID INT IDENTITY(1,1), '

	WHILE (@dim >= @i)

	BEGIN
		SET @TableCreate = @TableCreate + 'V' + CAST(@i AS VARCHAR(10)) + ' SMALLINT ,'
		SET @i = @i + 1
	END
	SET @TableCreate = STUFF(@TableCreate, LEN(@TableCreate), 1, ');')

	WHILE (@dim >= @j)
	BEGIN
		SET @TableCreate = @TableCreate + ' 
		INSERT INTO dbo.T_2048 VALUES ('
		+ STUFF(REPLICATE('0,',@dim), LEN(REPLICATE('0,',@dim)), 1, ');') 
		SET @j = @j+1
	END

	EXEC sp_executesql @tableCreate
END;
GO


--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- UPDATE positions for X,Y and VAL
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CREATE OR ALTER PROCEDURE dbo.UPDATE_position 
     @column INT
    ,@row INT
	,@val INT
AS
BEGIN

	DECLARE @COL NVARCHAR(100) = (
									SELECT 
										COLUMN_NAME 
									FROM INFORMATION_SCHEMA.COLUMNS
									WHERE
											TABLE_NAME = 'T_2048'
										AND TABLE_SCHEMA = 'dbo'
										AND ORDINAL_POSITION = @column+1 )

DECLARE @S NVARCHAR(2000) =  
	'UPDATE dbo.T_2048
		SET ' + CAST(@COL AS VARCHAR(100)) + ' = '+ CAST(@val AS VARCHAR(100)) +' 
		WHERE	
			ID = '+CAST(@row AS VARCHAR(100))

	EXEC sp_executesql @S
END;
GO


CREATE OR ALTER PROCEDURE dbo.INIT_matrix
	@dim INT
AS
BEGIN
	
declare @a int = 1
declare @b int = 4

declare @x int, @y int = 1

SET @x = (SELECT FLOOR(RAND()*(@b-@a+1))+@a)
SET @y = (SELECT FLOOR(RAND()*(@b-@a+1))+@a)

	DECLARE @COL NVARCHAR(100) = (
			SELECT 
				COLUMN_NAME 
			FROM INFORMATION_SCHEMA.COLUMNS
			WHERE
					TABLE_NAME = 'T_2048'
				AND TABLE_SCHEMA = 'dbo'
				AND ORDINAL_POSITION = @x+1 )



DECLARE @Sq NVARCHAR(2000) =  
	'UPDATE dbo.T_2048
		SET ' + CAST(@COL AS VARCHAR(100)) + ' = CASE WHEN ' + CAST(@COL AS VARCHAR(100)) + '  = 0 THEN 2  ELSE ' + CAST(@COL AS VARCHAR(100)) + '  END
		WHERE	
			ID = '+CAST(@y AS VARCHAR(100))

	EXEC sp_executesql @Sq

END;
GO



CREATE OR ALTER PROCEDURE dbo.MAKE_move
	 @move CHAR(1) -- U, D, L, R (Up, Down, Left, Right)
	,@dim  INT  -- size of the matrix
AS
BEGIN

	IF @move = 'U'
		BEGIN
			EXEC dbo.MOVE_up @dim
			SELECT * FROM T_2048
		END

	IF @move = 'D'
		BEGIN
			EXEC dbo.MOVE_down @dim
			SELECT * FROM T_2048
		END


	IF @move = 'L'
	SELECT 'L'
	IF @move = 'R'
	SELECT 'R'
END;
GO


-- ===========================================
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ===========================================
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Create
EXEC dbo.CREATE_matrix 4
-- Initialize
EXEC dbo.INIT_matrix 4  --( == EXEC dbo.ADD_number ) -- Add new number 2 on random empty place


SELECT * FROM T_2048

-- Play
EXEC dbo.MAKE_move 'D', 4





-- test
/*
DROP TABLE IF exists tt
create table tt (id int, v1 int)

--insert into tt 
--select 1,0 union all
--select 2,2 union all
--select 3,2 union all
--select 4,0

insert into tt 
select 1,2 union all
select 2,0 union all
select 3,2 union all
select 4,0

*/
-- ~~~~~~~~~~~~~~~~~~~
--  UP
-- ~~~~~~~~~~~~~~~~~~~~
DEcLAre @ii int = 1
while 4/2 >= @ii
BEGIN
	declare @i int = 1

	while 4 > @i
	begin	
		declare @vv_1 int = (select v1 from tt where id = @i)
		declare @vv_2 int = (select v1 from tt where id = @i+1)

	IF (@vv_1 = 0 AND @vv_2 <> 0)
	BEGIN
		update tt set v1 = @vv_2 where id = @i   -- EXEC dbo.UPDATE_position 1,@i,@vv_2
		update tt set v1 = 0     where id = @i+1 -- EXEC dbo.UPDATE_position 1,@i+1,0
	END

	IF (@vv_1 <> 0 AND @vv_1 = @vv_2)
	BEGIN
		update tt set v1 = @vv_1 + @vv_2 where id = @i
		update tt set v1 = 0 where id = @i+1
    END

	set @i = @i + 1
	end
  set @ii = @ii + 1
END


-- ~~~~~~~~~~~~~~~~~~~~~~
--  DOWN
-- ~~~~~~~~~~~~~~~~~~~~~~

DEcLAre @ii int = 1
while 4/2 >= @ii
BEGIN
			declare @i int = 4 -- dimenzija
			while 1 < @i
			begin	
				declare @vv_1 int = (select v1 from tt where id = @i)
				declare @vv_2 int = (select v1 from tt where id = @i-1)

			IF (@vv_1 = 0 AND @vv_2 <> 0)
			BEGIN
				update tt set v1 = @vv_2 where id = @i
				update tt set v1 = 0     where id = @i-1
			END

			IF (@vv_1 <> 0 AND @vv_1 = @vv_2)
			BEGIN
				update tt set v1 = @vv_1 + @vv_2 where id = @i
				update tt set v1 = 0 where id = @i-1
			END

			set @i = @i - 1
END
 -- SELECT @ii as iiverzija
  --select * from tt
  set @ii = @ii + 1
END



-- ~~~~~~~~~~~~~~~~~~~~~~
--  LEFT
-- ~~~~~~~~~~~~~~~~~~~~~~
/*

drop table if exists tt;
create table tt (id int, v1 int, v2 int, v3 int, v4 int);

insert into tt(id,v1,v2,v3,v4) values (1, 0, 0, 2, 2)


*/

drop table if exists #temp
select
row_number() over (order by (select 1)) as id
, v1
into #temp
from (
select v1 as v1 from tt
union all
select v2 from tt
union all 
select v3 from tt
union all
select v4 from tt
) as x

-- left == down

DEcLAre @ii int = 1
while (4-1) >= @ii  -- stevilo prehodov = n-1
BEGIN
			declare @i int = 4 -- dimenzija
			while 1 < @i
			begin	
				declare @vv_1 int = (select v1 from #temp where id = @i)
				declare @vv_2 int = (select v1 from #temp where id = @i-1)

			IF (@vv_1 = 0 AND @vv_2 <> 0)
			BEGIN
				update #temp set v1 = @vv_2 where id = @i
				update #temp set v1 = 0     where id = @i-1
			END

			IF (@vv_1 <> 0 AND @vv_1 = @vv_2)
			BEGIN
				update #temp set v1 = @vv_1 + @vv_2 where id = @i
				update #temp set v1 = 0 where id = @i-1
			END

			set @i = @i - 1
END
 -- SELECT @ii as iiverzija
  --select * from tt
  set @ii = @ii + 1
END


-- final update

select * from tt
select * from #temp

declare @y int = 1

while @y <= 4 -- variable dim
begin

	declare @val int = (select v1 from #temp where id = @y)

	declare @s nvarchar(500)
	set @s = 'UPDATE tt
			set v' + CAST(@y AS VARCHAR(10)) + '= ' + CAST(@val AS VARCHAR(10))

	EXEC sp_executesql @s

	set @y = @y + 1
end

select * from tt



-- ~~~~~~~~~~~~~~~~~~~~~~
--  RIGHT
-- ~~~~~~~~~~~~~~~~~~~~~~
/*
drop table if exists tt;
create table tt (id int, v1 int, v2 int, v3 int, v4 int);

insert into tt(id,v1,v2,v3,v4) values (1, 0, 0, 2, 2)


*/

select 
id
,v1
,v2
,v3
,v4 
from tt


drop table if exists #temp
select
row_number() over (order by (select 1)) as id
, v1
into #temp
from (
select v1 as v1 from tt
union all
select v2 from tt
union all 
select v3 from tt
union all
select v4 from tt
) as x

SELECT  * FROM #temp


-- right == up

DEcLAre @ii int = 1
while (4-1) >= @ii
BEGIN

	declare @i int = 1
	while 4 > @i
	begin	
		declare @vv_1 int = (select v1 from #temp where id = @i)
		declare @vv_2 int = (select v1 from #temp where id = @i+1)

	IF (@vv_1 = 0 AND @vv_2 <> 0)
	BEGIN
		update #temp set v1 = @vv_2 where id = @i  
		update #temp set v1 = 0     where id = @i+1 
	END

	IF (@vv_1 <> 0 AND @vv_1 = @vv_2)
	BEGIN
		update #temp set v1 = @vv_1 + @vv_2 where id = @i
		update #temp set v1 = 0 where id = @i+1
    END

	set @i = @i + 1
	end
  set @ii = @ii + 1
END



-- final update

--select * from tt
select * from #temp

declare @y int = 1

while @y <= 4 -- variable dim
begin

	declare @val int = (select v1 from #temp where id = @y)

	declare @s nvarchar(500)
	set @s = 'UPDATE tt
			set v' + CAST(@y AS VARCHAR(10)) + '= ' + CAST(@val AS VARCHAR(10))

	EXEC sp_executesql @s

	set @y = @y + 1
end

select * from tt




---- --------------------------
---- --------------------------
---- MOVE UP Procedure
---- --------------------------
---- --------------------------

CREATE OR ALTER PROCEDURE dbo.MOVE_up
		@dim INT
AS
BEGIN



	DECLARE @Column_counter INT = 2
	Declare @max_column INT = (SELECT @dim /* dim */ + 2)


	while @max_column > @Column_counter
	BEGIN
		-- Get first column name
		DECLARE @col_name VARCHAR(10) = (SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 'T_2048' and ORDINAL_POSITION = @Column_counter)
		print @col_name

		DECLARE @sql1 NVARCHAR(2000) =
		'SELECT  id, ' + @col_name + ' as v FROM T_2048'

		DROP TABLE IF EXISTS #temp;
		create table  #temp (id int, v int)
	
		insert into #temp
		EXEC sp_executesql @sql1

		--SELECT * FROM #temp
		/* premik */

		---------------------------------------
					-- UP scenarij
						DECLARE @ii int = 1
						while @dim-1 >= @ii -- �tevilo dimenzij
						BEGIN
							declare @i int = 1

							while @dim > @i -- 
							--declare @i int = 1 
							begin	
								declare @vv_1 int = (select v from #temp where id = @i)
								declare @vv_2 int = (select v from #temp where id = @i+1)

							IF (@vv_1 = 0 AND @vv_2 <> 0)
							BEGIN
								update #temp set v = @vv_2 where id = @i   
								update #temp set v = 0     where id = @i+1 
							END

							IF (@vv_1 <> 0 AND @vv_1 = @vv_2)
							BEGIN
								update #temp set v = @vv_1 + @vv_2 where id = @i
								update #temp set v = 0 where id = @i+1
							END

							IF (@vv_1 <> 0 AND @vv_2  = 0) 
							BEGIN
								Print 'Do nothing'
							END

							set @i = @i + 1
							end
						  set @ii = @ii + 1
						END

				-- update back to T_2048 table from #temp table

				DECLARE @sql_temp_update NVARCHAR(500)

				SET @SQL_temp_update = 
				'UPDATE T20
					SET '+@col_name+' = t.v
			
				FROM t_2048  AS T20
				JOIN #temp AS t
				ON T20.id = t.id'
			
				EXEC sp_executesql @SQL_temp_update

		----------------------------------------

		SET @Column_counter = @Column_counter + 1

		-- END; Show T_2048
	

	   END
END


---- --------------------------
---- --------------------------
---- MOVE DOWN Procedure
---- --------------------------
---- --------------------------


CREATE OR ALTER PROCEDURE dbo.MOVE_down
		@dim INT
AS
BEGIN

	DECLARE @Column_counter INT = 2
	Declare @max_column INT = (SELECT @dim /* dim */ + 2)


	while @max_column > @Column_counter
	BEGIN
		-- Get first column name
		DECLARE @col_name VARCHAR(10) = (SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 'T_2048' and ORDINAL_POSITION = @Column_counter)
		print @col_name

		DECLARE @sql1 NVARCHAR(2000) =
		'SELECT  id, ' + @col_name + ' as v FROM T_2048'

		DROP TABLE IF EXISTS #temp;
		create table  #temp (id int, v int)
	
		insert into #temp
		EXEC sp_executesql @sql1

		--SELECT * FROM #temp
		/* premik */

		---------------------------------------
					-- DOWN scenarij

						DECLARE @ii int = 1
						while @dim-1 >= @ii -- �tevilo dimenzij
						BEGIN
							declare @i int = @dim

							while 1 < @i  
							begin	
								declare @vv_1 int = (select v from #temp where id = @i)
								declare @vv_2 int = (select v from #temp where id = @i-1)

							IF (@vv_1 = 0 AND @vv_2 <> 0)
							BEGIN
								update #temp set v = @vv_2 where id = @i   
								update #temp set v = 0     where id = @i-1 
							END

							IF (@vv_1 <> 0 AND @vv_1 = @vv_2)
							BEGIN
								update #temp set v = @vv_1 + @vv_2 where id = @i
								update #temp set v = 0 where id = @i-1
							END

							IF (@vv_1 <> 0 AND @vv_2  = 0) 
							BEGIN
								Print 'Do nothing'
							END

							set @i = @i - 1
							end
						  set @ii = @ii + 1
						END

				-- update back to T_2048 table from #temp table

				DECLARE @sql_temp_update NVARCHAR(500)

				SET @SQL_temp_update = 
				'UPDATE T20
					SET '+@col_name+' = t.v
			
				FROM t_2048  AS T20
				JOIN #temp AS t
				ON T20.id = t.id'
			
				EXEC sp_executesql @SQL_temp_update

		----------------------------------------

		SET @Column_counter = @Column_counter + 1

		-- END; Show T_2048
	

	   END
END
