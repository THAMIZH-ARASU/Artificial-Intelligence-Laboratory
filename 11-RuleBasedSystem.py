# Smart Home Automation Rule-Based System
# Modular design with separate components for sensors, devices, rules, and control

from abc import ABC, abstractmethod
from enum import Enum
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from datetime import datetime, time
import json

# ============= ENUMS AND DATA CLASSES =============

class DeviceType(Enum):
    LIGHT = "light"
    THERMOSTAT = "thermostat"
    SECURITY_CAMERA = "security_camera"
    DOOR_LOCK = "door_lock"
    WINDOW_BLINDS = "window_blinds"
    AIR_PURIFIER = "air_purifier"

class SensorType(Enum):
    TEMPERATURE = "temperature"
    HUMIDITY = "humidity"
    LIGHT = "light"
    MOTION = "motion"
    AIR_QUALITY = "air_quality"
    OCCUPANCY = "occupancy"

@dataclass
class SensorReading:
    sensor_type: SensorType
    value: Any
    timestamp: datetime
    location: str

@dataclass
class DeviceCommand:
    device_id: str
    action: str
    parameters: Dict[str, Any]
    timestamp: datetime

# ============= SENSOR MODULE =============

class Sensor(ABC):
    def __init__(self, sensor_id: str, location: str):
        self.sensor_id = sensor_id
        self.location = location
        self.last_reading = None
    
    @abstractmethod
    def read(self) -> SensorReading:
        pass

class TemperatureSensor(Sensor):
    def __init__(self, sensor_id: str, location: str, current_temp: float = 22.0):
        super().__init__(sensor_id, location)
        self.current_temp = current_temp
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.TEMPERATURE,
            value=self.current_temp,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

class MotionSensor(Sensor):
    def __init__(self, sensor_id: str, location: str, motion_detected: bool = False):
        super().__init__(sensor_id, location)
        self.motion_detected = motion_detected
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.MOTION,
            value=self.motion_detected,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

class LightSensor(Sensor):
    def __init__(self, sensor_id: str, location: str, lux_level: int = 300):
        super().__init__(sensor_id, location)
        self.lux_level = lux_level
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.LIGHT,
            value=self.lux_level,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

class OccupancySensor(Sensor):
    def __init__(self, sensor_id: str, location: str, occupied: bool = False):
        super().__init__(sensor_id, location)
        self.occupied = occupied
    
    def read(self) -> SensorReading:
        reading = SensorReading(
            sensor_type=SensorType.OCCUPANCY,
            value=self.occupied,
            timestamp=datetime.now(),
            location=self.location
        )
        self.last_reading = reading
        return reading

# ============= DEVICE MODULE =============

class Device(ABC):
    def __init__(self, device_id: str, device_type: DeviceType, location: str):
        self.device_id = device_id
        self.device_type = device_type
        self.location = location
        self.is_on = False
        self.status = {}
    
    @abstractmethod
    def execute_command(self, command: DeviceCommand) -> bool:
        pass
    
    def get_status(self) -> Dict[str, Any]:
        return {
            "device_id": self.device_id,
            "type": self.device_type.value,
            "location": self.location,
            "is_on": self.is_on,
            "status": self.status
        }

class SmartLight(Device):
    def __init__(self, device_id: str, location: str):
        super().__init__(device_id, DeviceType.LIGHT, location)
        self.brightness = 100
        self.color = "white"
    
    def execute_command(self, command: DeviceCommand) -> bool:
        try:
            if command.action == "turn_on":
                self.is_on = True
                self.brightness = command.parameters.get("brightness", 100)
                self.color = command.parameters.get("color", "white")
            elif command.action == "turn_off":
                self.is_on = False
            elif command.action == "set_brightness":
                self.brightness = command.parameters.get("brightness", 100)
            elif command.action == "set_color":
                self.color = command.parameters.get("color", "white")
            
            self.status = {"brightness": self.brightness, "color": self.color}
            return True
        except Exception as e:
            print(f"Error executing command on {self.device_id}: {e}")
            return False

class Thermostat(Device):
    def __init__(self, device_id: str, location: str):
        super().__init__(device_id, DeviceType.THERMOSTAT, location)
        self.target_temp = 22.0
        self.mode = "auto"  # auto, heat, cool, off
    
    def execute_command(self, command: DeviceCommand) -> bool:
        try:
            if command.action == "set_temperature":
                self.target_temp = command.parameters.get("temperature", 22.0)
                self.is_on = True
            elif command.action == "set_mode":
                self.mode = command.parameters.get("mode", "auto")
            elif command.action == "turn_off":
                self.is_on = False
                self.mode = "off"
            
            self.status = {"target_temp": self.target_temp, "mode": self.mode}
            return True
        except Exception as e:
            print(f"Error executing command on {self.device_id}: {e}")
            return False

class SecurityCamera(Device):
    def __init__(self, device_id: str, location: str):
        super().__init__(device_id, DeviceType.SECURITY_CAMERA, location)
        self.recording = False
        self.motion_detection = True
    
    def execute_command(self, command: DeviceCommand) -> bool:
        try:
            if command.action == "start_recording":
                self.recording = True
                self.is_on = True
            elif command.action == "stop_recording":
                self.recording = False
            elif command.action == "enable_motion_detection":
                self.motion_detection = True
            elif command.action == "disable_motion_detection":
                self.motion_detection = False
            elif command.action == "turn_off":
                self.is_on = False
                self.recording = False
            
            self.status = {"recording": self.recording, "motion_detection": self.motion_detection}
            return True
        except Exception as e:
            print(f"Error executing command on {self.device_id}: {e}")
            return False

# ============= RULE ENGINE MODULE =============

class Rule(ABC):
    def __init__(self, rule_id: str, priority: int = 1):
        self.rule_id = rule_id
        self.priority = priority
        self.enabled = True
    
    @abstractmethod
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        pass

class TemperatureRule(Rule):
    def __init__(self, rule_id: str, target_temp: float, tolerance: float = 2.0):
        super().__init__(rule_id, priority=2)
        self.target_temp = target_temp
        self.tolerance = tolerance
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Find temperature sensors
        temp_readings = [reading for reading in sensor_data.values() 
                        if reading.sensor_type == SensorType.TEMPERATURE]
        
        for reading in temp_readings:
            current_temp = reading.value
            location = reading.location
            
            # Find thermostat in the same location
            thermostats = [device for device in device_states.values() 
                          if device.device_type == DeviceType.THERMOSTAT and 
                          device.location == location]
            
            for thermostat in thermostats:
                if abs(current_temp - self.target_temp) > self.tolerance:
                    command = DeviceCommand(
                        device_id=thermostat.device_id,
                        action="set_temperature",
                        parameters={"temperature": self.target_temp},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

class MotionLightRule(Rule):
    def __init__(self, rule_id: str):
        super().__init__(rule_id, priority=3)
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Find motion sensors
        motion_readings = [reading for reading in sensor_data.values() 
                          if reading.sensor_type == SensorType.MOTION]
        
        for reading in motion_readings:
            location = reading.location
            motion_detected = reading.value
            
            # Find lights in the same location
            lights = [device for device in device_states.values() 
                     if device.device_type == DeviceType.LIGHT and 
                     device.location == location]
            
            for light in lights:
                if motion_detected and not light.is_on:
                    command = DeviceCommand(
                        device_id=light.device_id,
                        action="turn_on",
                        parameters={"brightness": 80},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
                elif not motion_detected and light.is_on:
                    # Turn off light after 5 minutes (simplified for demo)
                    command = DeviceCommand(
                        device_id=light.device_id,
                        action="turn_off",
                        parameters={},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

class SecurityRule(Rule):
    def __init__(self, rule_id: str):
        super().__init__(rule_id, priority=1)  # High priority
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Check occupancy sensors
        occupancy_readings = [reading for reading in sensor_data.values() 
                             if reading.sensor_type == SensorType.OCCUPANCY]
        
        for reading in occupancy_readings:
            location = reading.location
            occupied = reading.value
            
            # Find security cameras in the same location
            cameras = [device for device in device_states.values() 
                      if device.device_type == DeviceType.SECURITY_CAMERA and 
                      device.location == location]
            
            for camera in cameras:
                if not occupied and not camera.recording:
                    # Start recording when area is unoccupied
                    command = DeviceCommand(
                        device_id=camera.device_id,
                        action="start_recording",
                        parameters={},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

class AmbientLightRule(Rule):
    def __init__(self, rule_id: str, low_light_threshold: int = 200):
        super().__init__(rule_id, priority=2)
        self.low_light_threshold = low_light_threshold
    
    def evaluate(self, sensor_data: Dict[str, SensorReading], 
                 device_states: Dict[str, Device]) -> List[DeviceCommand]:
        commands = []
        
        # Find light sensors
        light_readings = [reading for reading in sensor_data.values() 
                         if reading.sensor_type == SensorType.LIGHT]
        
        for reading in light_readings:
            location = reading.location
            lux_level = reading.value
            
            # Find lights in the same location
            lights = [device for device in device_states.values() 
                     if device.device_type == DeviceType.LIGHT and 
                     device.location == location]
            
            for light in lights:
                if lux_level < self.low_light_threshold and not light.is_on:
                    # Turn on light with appropriate brightness
                    brightness = max(30, min(100, 100 - (lux_level / 10)))
                    command = DeviceCommand(
                        device_id=light.device_id,
                        action="turn_on",
                        parameters={"brightness": int(brightness)},
                        timestamp=datetime.now()
                    )
                    commands.append(command)
        
        return commands

# ============= RULE ENGINE =============

class RuleEngine:
    def __init__(self):
        self.rules: List[Rule] = []
    
    def add_rule(self, rule: Rule):
        self.rules.append(rule)
        # Sort by priority (higher priority first)
        self.rules.sort(key=lambda r: r.priority, reverse=True)
    
    def remove_rule(self, rule_id: str):
        self.rules = [rule for rule in self.rules if rule.rule_id != rule_id]
    
    def evaluate_all(self, sensor_data: Dict[str, SensorReading], 
                     device_states: Dict[str, Device]) -> List[DeviceCommand]:
        all_commands = []
        
        for rule in self.rules:
            if rule.enabled:
                try:
                    commands = rule.evaluate(sensor_data, device_states)
                    all_commands.extend(commands)
                except Exception as e:
                    print(f"Error evaluating rule {rule.rule_id}: {e}")
        
        return all_commands

# ============= MAIN SYSTEM CONTROLLER =============

class SmartHomeController:
    def __init__(self):
        self.sensors: Dict[str, Sensor] = {}
        self.devices: Dict[str, Device] = {}
        self.rule_engine = RuleEngine()
        self.sensor_data: Dict[str, SensorReading] = {}
        self.command_history: List[DeviceCommand] = []
    
    def add_sensor(self, sensor: Sensor):
        self.sensors[sensor.sensor_id] = sensor
    
    def add_device(self, device: Device):
        self.devices[device.device_id] = device
    
    def add_rule(self, rule: Rule):
        self.rule_engine.add_rule(rule)
    
    def read_all_sensors(self):
        """Read data from all sensors"""
        for sensor_id, sensor in self.sensors.items():
            reading = sensor.read()
            self.sensor_data[sensor_id] = reading
    
    def execute_commands(self, commands: List[DeviceCommand]):
        """Execute device commands"""
        for command in commands:
            if command.device_id in self.devices:
                device = self.devices[command.device_id]
                success = device.execute_command(command)
                if success:
                    self.command_history.append(command)
                    print(f"✓ Executed: {command.action} on {command.device_id}")
                else:
                    print(f"✗ Failed to execute: {command.action} on {command.device_id}")
    
    def run_automation_cycle(self):
        """Main automation cycle"""
        print("\n=== Starting Automation Cycle ===")
        
        # 1. Read all sensors
        self.read_all_sensors()
        print(f"Read {len(self.sensor_data)} sensor readings")
        
        # 2. Evaluate rules
        commands = self.rule_engine.evaluate_all(self.sensor_data, self.devices)
        print(f"Generated {len(commands)} commands")
        
        # 3. Execute commands
        if commands:
            self.execute_commands(commands)
        
        print("=== Automation Cycle Complete ===\n")
    
    def get_system_status(self) -> Dict[str, Any]:
        """Get current system status"""
        return {
            "sensors": {sid: sensor.last_reading.__dict__ if sensor.last_reading else None 
                       for sid, sensor in self.sensors.items()},
            "devices": {did: device.get_status() for did, device in self.devices.items()},
            "rules": len(self.rule_engine.rules),
            "last_commands": [cmd.__dict__ for cmd in self.command_history[-5:]]
        }

# ============= DEMO USAGE =============

def main():
    # Initialize the smart home system
    home = SmartHomeController()
    
    # Add sensors
    home.add_sensor(TemperatureSensor("temp_living", "living_room", 18.0))
    home.add_sensor(MotionSensor("motion_living", "living_room", True))
    home.add_sensor(LightSensor("light_living", "living_room", 150))
    home.add_sensor(OccupancySensor("occupancy_living", "living_room", False))
    
    home.add_sensor(TemperatureSensor("temp_bedroom", "bedroom", 25.0))
    home.add_sensor(MotionSensor("motion_bedroom", "bedroom", False))
    
    # Add devices
    home.add_device(SmartLight("light_living_main", "living_room"))
    home.add_device(SmartLight("light_bedroom_main", "bedroom"))
    home.add_device(Thermostat("thermostat_main", "living_room"))
    home.add_device(SecurityCamera("camera_living", "living_room"))
    
    # Add rules
    home.add_rule(TemperatureRule("temp_control", target_temp=22.0))
    home.add_rule(MotionLightRule("motion_lights"))
    home.add_rule(SecurityRule("security_monitoring"))
    home.add_rule(AmbientLightRule("ambient_lighting", low_light_threshold=200))
    
    # Run automation cycles
    for cycle in range(3):
        print(f"\n{'='*50}")
        print(f"AUTOMATION CYCLE {cycle + 1}")
        print(f"{'='*50}")
        
        home.run_automation_cycle()
        
        # Show system status
        status = home.get_system_status()
        print("\n--- System Status ---")
        print(f"Active Devices: {len([d for d in status['devices'].values() if d['is_on']])}")
        print(f"Total Rules: {status['rules']}")
        
        # Simulate some changes for next cycle
        if cycle == 0:
            # Simulate motion detection
            home.sensors["motion_bedroom"].motion_detected = True
            home.sensors["temp_bedroom"].current_temp = 19.0
        elif cycle == 1:
            # Simulate occupancy change
            home.sensors["occupancy_living"].occupied = True
            home.sensors["light_living"].lux_level = 50

if __name__ == "__main__":
    main()