SET TOOL_PATH=.fake

SET PAKET_PATH=.paket

IF NOT EXIST "%PAKET_PATH%\paket.exe" (
  dotnet tool install paket --tool-path ./%PAKET_PATH%
)

IF NOT EXIST "%TOOL_PATH%\fake.exe" (
  dotnet tool install fake-cli --tool-path ./%TOOL_PATH%
)

"%PAKET_PATH%/paket.exe" install